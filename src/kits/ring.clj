(ns ^{:doc "Useful fns for use with Ring."}
  kits.ring
  (:require [clojure.string :as str]))

;;; Response Modifiers

(defn disable-caching [response]
  (update-in response
             [:headers]
             merge
             {"Pragma" "no-cache"
              "Expires" "0"
              "Cache-Control" ["must-revalidate"
                               "no-cache"
                               "no-store"
                               "max-age=0"
                               "private"]}))

(defn enable-downable-as [response filename]
  (assoc-in response
            [:headers "content-disposition"]
            (str "attachment;filename=" filename)))

;;; Response Creation

(defn wrong-method-response [expected-methods actual-method]
  {:body (str "Wups you "
              (str/upper-case (name actual-method))
              "ed. This url only accepts these methods: "
              (->> expected-methods
                   (map (comp str/upper-case name))
                   (str/join ", or")))
   :status 405})

(defn no-handler-response [message]
  {:status 404
   :headers {"content-type" "text/plain"}
   :body message})

(defn text-plain-response [s]
  {:status 200
   :body s
   :headers {"content-type" "text/plain"}})

(defn ok-response []
  (text-plain-response "OK"))

(defn json-response [json-str]
  {:status 200
   :body json-str
   :headers {"content-type" "application/json;charset=UTF-8"}})

(def edn-response text-plain-response)

(defn html-response [html-str]
  {:status 200
   :body html-str
   :headers {"content-type" "text/html"}})

(defn csv-response [csv-str]
  {:status 200
   :body csv-str
   :headers {"content-type" "text/csv"}})

(def text-plain-response-no-cache (comp disable-caching text-plain-response))
(def json-response-no-cache       (comp disable-caching json-response))
(def csv-response-no-cache        (comp disable-caching csv-response))
(def edn-response-no-cache        (comp disable-caching edn-response))
(def html-response-no-cache       (comp disable-caching html-response))

(defn response? [x]
  (and
   (map? x)
   (contains? x :status)
   (contains? x :body)))
