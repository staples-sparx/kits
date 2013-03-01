(ns kits.contextual-logging
  "A Context layer built on top of 'org.rathore.amit.utils.logger'"
  (:require [clojure.string :as str]
            [org.rathore.amit.utils.logger :as amit]))


(def ^{:dynamic true
       :doc "Used internally by kits.contextual-logging, to maintain the current logging context."}
  *log-context* {})

(defn- stringify-context [m]
  (if (empty? m)
    ""
    (->> m
         reverse
         (map (fn [[k v]] (str (name k) ": " v)))
         (str/join ", ")
         (#(str % " - ")))))

(defn log-message
  "Delegates to amit/log-message, appending the context to the messages"
  [& message-tokens]
  (apply amit/log-message (str (stringify-context *log-context*) (first message-tokens)) (rest message-tokens)))

(defn log-exception
  "Delegates to amit/log-message, appending the context to the messages"
  ([e additional-message]
    (amit/log-exception e (str (stringify-context *log-context*) additional-message)))
  ([e]
    (let [context (stringify-context *log-context*)]
      (if (= "" context)
        (amit/log-exception e)
        (amit/log-exception e context)))))

(defmacro in-log-context
  "Any calls to contextual-logging/log-message or contextual-logging/log-exception
   will have the surrounding context added"
  [log-context-map & body]
  `(binding [kits.contextual-logging/*log-context* (merge kits.contextual-logging/*log-context* ~log-context-map)]
     ~@body))