(ns kits.structured-logging
  "Logging Clojure data as JSON"
  (:require [clojure.tools.logging :as log]
            [cheshire.custom :as cc]
            [kits.homeless :as hl]
            [kits.timestamp :as ts]))


(def ^{:dynamic true
       :doc "Used internally by kits.structured-logging, to maintain the current logging context."}
  *log-context* nil)

(defn unmangle
  "Given the name of a class that implements a Clojure function, returns the function's name in Clojure."
  [class-name]
  (.replace
    (clojure.string/replace class-name #"^(.+)\$(.+)$" "$1/$2") \_ \-))

(defmacro current-function-name
  "Returns the name of the current Clojure function."
  []
  `(let [^StackTraceElement element# (-> (Throwable.)
                                        .getStackTrace
                                        first)]
     (unmangle (.getClassName element#))))

;; logs assorted Objects sanely: good for logging functions or
;; assorted objects
(cc/add-encoder Object cc/encode-str)

(defn structured-log*
  "Used internally by kits.structured-logging"
  [log-level tags the-ns calling-fn-name log-map]
  (log/logp log-level (cc/encode (merge {:level log-level
                                         :timestamp (ts/->str (ts/now))
                                         :function calling-fn-name
                                         :namespace (str the-ns)
                                         :data log-map}
                                        (when tags
                                          {:tags (vec tags)})
                                        (when *log-context*
                                          {:context *log-context*})))))


(defmacro info
  "Log level info. Logs `log-map` param as JSON, appending any surrounding
   context from `in-context` and adds any supplied :tags."
  [log-map & {:keys [tags]}]
  `(structured-log* :info ~tags ~*ns* (current-function-name) ~log-map))

(defmacro warn
  "Log level warn. Logs `log-map` param as JSON, appending any surrounding
   context from `in-context` and adds any supplied :tags."
  [log-map & {:keys [tags]}]
  `(structured-log* :warn ~tags ~*ns* (current-function-name) ~log-map))

(defmacro error
  "Log level error. Logs `log-map` param as JSON, appending any surrounding
   context from `in-context` and adds any supplied :tags."
  [log-map & {:keys [tags]}]
  `(structured-log* :error ~tags ~*ns* (current-function-name) ~log-map))

(defmacro in-context
  "Any calls to structured-logging info, warn or error macros
   will have the surrounding context added"
  [log-context-map & body]
  `(binding [kits.structured-logging/*log-context* (merge kits.structured-logging/*log-context* ~log-context-map)]
     ~@body))

(defn log-time*
  "Higher order function version of `log-time` macro"
  [tag extra-info-map body-fn]
  (let [start-ms (ts/now)]
    (info {:start start-ms
           :start-pretty (ts/->str start-ms)}
          :tags [(keyword (str (name tag) "-timing-start"))])

    (let [result (body-fn)
          millis-elapsed (- (ts/now) start-ms)]
      (info {:start start-ms
             :start-pretty (ts/->str start-ms)
             :millis-elapsed millis-elapsed
             :extra-info extra-info-map}
            :tags [(keyword (str (name tag) "-timing-summary"))])
      result)))

(defmacro log-time
  "Process the body, and log info about how long it took."
  [tag extra-info-map & body]
  `(log-time* ~tag ~extra-info-map (fn [] ~@body)))

(defn logging-exceptions*
  "Higher order function version of `logging-exceptions` macro"
  [body-fn]
  (try
    (body-fn)
    (catch Throwable e
      (error {:exception-message (str e)
              :stacktrace (hl/stacktrace->str e)})
      (throw e))))

(defmacro logging-exceptions
  "Executes the body. Any Throwables are logged then re-thrown."
  [& body]
  `(logging-exceptions* (fn [] ~@body)))

