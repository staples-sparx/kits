(ns kits.structured-logging
  "Logging Clojure data as JSON"
  (:require [cheshire.custom :as cc]
            [clojure.string :as str]
            [kits.homeless :as hl]
            [kits.map :as m]
            [kits.syslog :as syslog]
            [kits.timestamp :as ts]
            [runa.tools.logging :as log])
  (:import kits.syslog.udp.Channel))

(set! *warn-on-reflection* true)

(def ^ThreadLocal syslog-channel (ThreadLocal.))

(def ^{:dynamic true
       :doc "Used internally by kits.structured-logging, to maintain the current logging context."}
  *log-context* {:data {}
                 :tags []})

(defn log-context
  "Context with :tags as a top-level key.  Useful for transporting
   the context across processes, to be passed back into
   in-log-context at a later point"
  []
  (merge (:data *log-context*)
         {:tags (:tags *log-context*)}))

;; logs assorted Objects sanely: good for logging functions or
;; assorted objects
(cc/add-encoder Object cc/encode-str)

(defn structured-log*
  "Used internally by kits.structured-logging"
  [syslog-config local-name log-level tags log-map]
  (let [all-tags (vec (distinct (into (:tags *log-context*) tags)))
        facility (syslog/facilities "local0")
        level (syslog/levels log-level)
        channel (or (.get syslog-channel)
                    (syslog/create-channel syslog-config))
        msg (cc/encode (merge {:level (str/upper-case (name log-level))
                               :ts-ms (ts/now)
                               :data log-map}
                              (when-not (empty? all-tags)
                                {:tags all-tags})
                              (when-not (empty? (:data *log-context*))
                                {:context (m/map-values #(if (fn? %) (%) %)
                                                        (:data *log-context*))})))
        fresh-channel (syslog/log syslog-config facility local-name level channel msg)]
    (.set syslog-channel fresh-channel)))

(defn stacktrace
  "Get stacktrace string from an exception"
  [^Throwable e]
  (str/join "\n" (list* (-> e .getClass .getName)
                        (.getMessage e)
                        (.getStackTrace e))))

(defn root-cause
  "Root cause of an exception, if one exists"
  [^Throwable exception]
  (->> exception
       (iterate #(.getCause ^Throwable %))
       (take-while (complement nil?))
       last))

(defmacro safe-eval-log-map
  "Implementation detail."
  [syslog-config local-name log-map]
  `(try
     ~log-map
     (catch Exception e#
       (structured-log* ~syslog-config
                        ~local-name
                        :error
                        [:alert :logging :exception]
                        {:log-map (str '~log-map)
                         :stacktrace (stacktrace (root-cause e#))})
       nil)))

(defmacro info
  "Log level info. Logs `log-map` param as JSON, appending any surrounding
   context from `in-log-context` and adds any supplied :tags."
  [syslog-config local-name log-map]
  `(when-let [log-map# (safe-eval-log-map ~syslog-config ~local-name ~log-map)]
     (structured-log* ~syslog-config ~local-name :info (:tags log-map#) (dissoc log-map# :tags))))

(defmacro warn
  "Log level warn. Logs `log-map` param as JSON, appending any surrounding
   context from `in-log-context` and adds any supplied :tags."
  [syslog-config local-name log-map]
  `(when-let [log-map# (safe-eval-log-map ~syslog-config ~local-name ~log-map)]
     (structured-log* ~syslog-config ~local-name :warn (:tags log-map#) (dissoc log-map# :tags))))

(defmacro error
  "Log level error. Logs `log-map` param as JSON, appending any surrounding
   context from `in-log-context` and adds any supplied :tags."
  [syslog-config local-name log-map]
  `(when-let [log-map# (safe-eval-log-map ~syslog-config ~local-name ~log-map)]
     (structured-log* ~syslog-config ~local-name :error (:tags log-map#) (dissoc log-map# :tags))))

(defn exception
  "Log an exception at log level of error."
  ([syslog-config local-name ^Throwable exception]
     (exception syslog-config local-name ^Throwable exception {}))
  ([syslog-config local-name ^Throwable exception log-map]
     (let [root (root-cause exception)]
       (error syslog-config
              local-name
              (-> log-map
                  (assoc :log/exception (hl/exception->map exception))
                  (update-in [:tags] #(conj (or % []) :exception)))))))

(defmacro in-log-context
  "Any calls to structured-logging info, warn or error macros
   will have the surrounding context added. Context map values can be no-arg
   functions that get evaluated at log-time."
  [log-context-map & body]
  `(let [log-context-map# ~log-context-map]
     (binding [*log-context* {:data (merge (dissoc log-context-map# :tags) (:data *log-context*))
                              :tags (into (:tags *log-context*) (:tags log-context-map#))}]
       ~@body)))

(defn log-time*
  "Higher order function version of `log-time` macro"
  [syslog-config local-name tag extra-info-map body-fn]
  (let [start-ms (ts/now)]
    (info syslog-config local-name {:start start-ms
                                    :start-pretty (ts/->str start-ms)
                                    :tags [(keyword (str (name tag) "-timing-start"))]})

    (let [result (body-fn)
          millis-elapsed (- (ts/now) start-ms)]
      (info syslog-config local-name {:start start-ms
                                      :start-pretty (ts/->str start-ms)
                                      :millis-elapsed millis-elapsed
                                      :extra-info extra-info-map
                                      :tags [(keyword (str (name tag) "-timing-summary"))]})
      result)))

(defmacro log-time
  "Process the body, and log info about how long it took."
  [syslog-config local-name tag extra-info-map & body]
  `(log-time* ~syslog-config ~local-name ~tag ~extra-info-map (fn [] ~@body)))

(defn logging-exceptions*
  "Higher order function version of `logging-exceptions` macro"
  [syslog-config local-name body-fn]
  (try
    (body-fn)
    (catch Throwable e
      (error syslog-config local-name {:exception-message (str e)
                                       :stacktrace (hl/stacktrace->str e)})
      (throw e))))

(defmacro logging-exceptions
  "Executes the body. Any Throwables are logged then re-thrown."
  [syslog-config local-name & body]
  `(logging-exceptions* ~syslog-config ~local-name (fn [] ~@body)))
