(ns kits.contextual-logging
  "A Context layer built on top of 'org.rathore.amit.utils.logger'"
  (:require [clojure.string :as str]
            [kits.queues :as q]
            [kits.thread :as thread]
            [org.rathore.amit.utils.logger :as amit]))


;;; Synchronous Logging

(def ^{:dynamic true
       :doc "Used internally by kits.contextual-logging, to maintain the current logging context."}
  *log-context* {})

(defmacro in-log-context
  "Any calls to contextual-logging/log-message or contextual-logging/log-exception
   will have the surrounding context added"
  [log-context-map & body]
  `(binding [kits.contextual-logging/*log-context* (merge kits.contextual-logging/*log-context* ~log-context-map)]
     ~@body))

(defn stringify
  "Turn a map into key-value pairs.  Useful for logging Clojure code."
  [m]
  (if (empty? m)
    ""
    (->> m
         reverse
         (map (fn [[k v]] (str (name k) ": " v)))
         (str/join ", ")
         (#(str % " - ")))))

(defn- log-message-with-context [log-context & message-tokens]
  (apply amit/log-message (str (stringify log-context) (first message-tokens)) (rest message-tokens)))

(defn log-message
  "Delegates to amit/log-message, appending the context to the messages"
  [& message-tokens]
  (apply log-message-with-context *log-context* message-tokens))

(defn- log-exception-with-context
  ([log-context e additional-message]
    (amit/log-exception e (str (stringify log-context) additional-message)))
  ([log-context e]
    (let [context (stringify log-context)]
      (if (= "" context)
        (amit/log-exception e)
        (amit/log-exception e context)))))

(defn log-exception
  "Delegates to amit/log-message, appending the context to the messages"
  ([e additional-message]
     (log-exception-with-context *log-context* e additional-message))
  ([e]
     (log-exception-with-context *log-context* e)))


;;; Async Logging

(def async-logging-queue
  "Holds the Java ArrayBlockingQueue for high throughput, async logging."
  (atom nil))

(defn- log-message-consumer [_thread-name_ _args_]
  (while true
    (when-let [f (q/fetch @async-logging-queue)]
      (f))))

(defn init-async-logging-queue
  "Java ArrayBlockingQueue for high throughput, async logging.
   Queue 'capacity' defaults to 500"
  [& {:keys [capacity]
      :or {capacity 500}}]
  (reset! async-logging-queue (q/create capacity)))

(defn init-thread-pool
  "Inits the asynchronous logging thread pool."
  [& {:keys [thread-count]
      :or {thread-count 1}}]
  (thread/start-thread-pool thread-count "async-logging-queue" log-message-consumer))

(defn async-log-message
  "Like `log-message`, but executes logging on a dedicated background thread."
  [& message-tokens]
  (let [log-context *log-context*]
    (q/add @async-logging-queue (fn []
                                  (apply log-message-with-context log-context message-tokens)))))

(defn async-log-exception
  "Like `log-exception`, but executes logging on a dedicated background thread."
  ([e additional-message]
     (let [log-context *log-context*]
       (q/add @async-logging-queue (fn []
                                     (log-exception-with-context log-context e additional-message)))))
  ([e]
     (let [log-context *log-context*]
       (q/add @async-logging-queue (fn []
                                     (log-exception-with-context log-context e))))))

