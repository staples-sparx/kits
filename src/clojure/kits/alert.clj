(ns ^{:doc "PagerDuty alerts"}
  kits.alert
  (:require
    [clojure.core.async :as async]
    [clj-http.client :as http]
    [kits.logging.log-async :as log]
    [kits.logging.log-generator :as log-gen]
    [kits.json :as json]
    [kits.map :refer [filter-map]]
    [kits.runtime :as runtime]
    [kits.timestamp :as ts]))

(def ^:private default-config
  {:mode :log
   :pagerduty-api-endpoint "https://events.pagerduty.com/generic/2010-04-15/create_event.json"
   :pagerduty-api-key nil
   :buffer-size 3 ; buffer n alerts (drop the rest)
   :throttle-ms 30000 ; Rate limit consumption of alerts (only applies to :pagerduty mode)
   :alert-callback nil ; optional callback function that receives alert-info
   })

(defonce ^:private global-handler (atom nil))

(defn- log-alert
  [alert-info]
  {:pre [(map? alert-info) (some? (:description alert-info))]}
  (log/warn (merge
              {:message "Alert!"
               :alert true}
              alert-info)))

(defn- record-alert
  [{:keys [alert-callback]} alert-info]
  (log-alert alert-info)
  (when alert-callback
    (alert-callback alert-info)))

(defn- post-pagerduty-alert
  [{:keys [throttle-ms pagerduty-api-endpoint pagerduty-api-key alert-callback] :as config}
   {:keys [description details] :as alert-info}]
  (record-alert config alert-info)
  (try
    (let [body {:service_key pagerduty-api-key
                :event_type "trigger"
                :description description}
          body (if (some? details)
                 (assoc body :details details)
                 body)]
      (http/post pagerduty-api-endpoint
                 {:content-type :json
                  :body (json/encode-str body)
                  :throw-exceptions true})
      (async/<!! (async/timeout throttle-ms)))
    (catch Exception e
      (log/exception e {:message "Posting to pagerduty failed!"}))))

(defn- alert-handler
  [alert-config]
  (case (:mode alert-config)
    :pagerduty (partial post-pagerduty-alert alert-config)
    (partial record-alert alert-config)))

(defn- receive-alerts-thread
  [trigger-chan post-fn]
  (async/thread
    (loop []
      (when-let [alert-msg (async/<!! trigger-chan)]
        (post-fn alert-msg)
        (recur)))))

(defn create [handler-config]
  "Create an alert handler."
  (let [handler-config (merge default-config handler-config)
        trigger-chan (async/chan (async/dropping-buffer (:buffer-size handler-config)))
        post-fn (alert-handler handler-config)]
    {:trigger-chan trigger-chan
     :thread (receive-alerts-thread trigger-chan post-fn)}))

(defn destroy [handler]
  "Stop and destroy an alert handler."
  (when (some? handler)
    (async/close! (:trigger-chan handler))
    (async/<!! (:thread handler)) ; wait for thread to complete
    nil))

(defn initialize!
  "Initialize the global alert handler.
   When no :mode is specified, alerts are only logged on the local machine."
  [alert-config]
  (reset! global-handler (create alert-config)))

(defn uninitialize!
  "Uninitialize the global alert handler."
  []
  (swap! global-handler destroy))

(defn- json-encodable?
  [value]
  (try
    (string? (json/encode-str value))
    (catch Exception e
      false)))

(defn- sanitize-ex-data
  [dict]
  (filter-map (fn [k v] (json-encodable? [k v])) dict))

(defn exception-details
  [throwable]
  (let [data (ex-data throwable)
        details
        {:type (.getName (type throwable))
         :message (.getMessage throwable)
         :stacktrace (map str (.getStackTrace throwable))}
        details (if (some? data) (assoc details :data (sanitize-ex-data data)) details)
        inner (.getCause throwable)]
    (if (some? inner)
      (assoc details :cause (exception-details inner))
      details)))

(defn alert-details
  []
  (runtime/process-info))

(defn alert!
  "Raise an alert (to PagerDuty)"
  ([message] (alert! @global-handler message))
  ([handler message]
   (let [alert-info (cond
                      (instance? Throwable message)
                      {:description (.getMessage message)
                       :details (exception-details message)}

                      (map? message)
                      {:description (if-let [msg-str (:description message)]
                                      msg-str
                                      "Alert!")
                       :details (dissoc message :description)}

                      :else {:description (str message)})
         alert-info (update-in alert-info [:details] merge (alert-details))]
     (if (some? handler)
       (async/put! (:trigger-chan handler) alert-info)
       (log/warn {:message "No alert handler installed"
                  :alert alert-info})))))
