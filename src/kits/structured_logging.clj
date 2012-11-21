(ns ^{:doc "Logging with Clojure data"}
  kits.structured-logging
  (:require [clojure.tools.logging :as log]
            [cheshire.custom :as cc]
            [kits.homeless :as hl]
            [kits.timestamp :as ts]))


(def ^:dynamic *log-context* nil)

(defn unmangle
  "Given the name of a class that implements a Clojure function, returns the function's name in Clojure."
  [class-name]
  (.replace
    (clojure.string/replace class-name #"^(.+)\$(.+)$" "$1/$2") \_ \-))

(defmacro current-function-name
  "Returns a string, the name of the current Clojure function."
  []
  `(-> (Throwable.)
       .getStackTrace
       first
       .getClassName
       unmangle))

;; logs assorted Objects sanely: good for logging functions or
;; assorted objects
(cc/add-encoder Object cc/encode-str)

(defn structured-log [log-level tags the-ns calling-fn-name log-map]
  (log/logp log-level (cc/encode (merge {:tags (vec tags)
                                         :level log-level
                                         :function calling-fn-name
                                         :namespace (str the-ns)
                                         :data log-map}
                                        (when *log-context*
                                          {:context *log-context*})))))


;; Use these for logging. The above are public because I can't
;; get these funky macros to work with them private

(defmacro info [log-map & {:keys [tags]}]
  `(structured-log :info ~tags ~*ns* (current-function-name) ~log-map))

(defmacro warn [log-map & {:keys [tags]}]
  `(structured-log :warn ~tags ~*ns* (current-function-name) ~log-map))

(defmacro error [log-map & {:keys [tags]}]
  `(structured-log :error ~tags ~*ns* (current-function-name) ~log-map))

(defmacro in-context
  "Any calls to structured-logging info, warn or error macros
   will have the surrounding context added"
  [log-context-map & body]
  `(binding [kits.structured-logging/*log-context* (merge kits.structured-logging/*log-context* ~log-context-map)]
     ~@body))

(defmacro log-time [tag extra-info-map & body]
  `(let [start-ms# (ts/now)]
     (log/info {:start start-ms#
                :start-pretty (ts/->str start-ms#)}
               :tags [~(keyword (str (name tag) "-timing-start"))])

     (let [result# (do ~@body)
           millis-elapsed# (- (ts/now) start-ms#)]
       (log/info {:start start-ms#
                  :start-pretty (ts/->str start-ms#)
                  :millis-elapsed millis-elapsed#
                  :extra-info ~extra-info-map}
                 :tags [~(keyword (str (name tag) "-timing-summary"))])
       result#)))

(defmacro logging-exceptions [& body]
  `(try
     ~@body
     (catch Throwable e#
       (log/error {:exception-message (str e#)
                   :stacktrace (hl/stacktrace->str e#)})
       (throw e#))))

