(ns kits.contextual-logging
  "A Context layer built on top of 'org.rathore.amit.utils.logger'"
  (:require [clojure.string :as str]
            [kits.milliseconds-constants :as msc]
            [kits.queues :as q]
            [kits.thread :as thread]
            [org.rathore.amit.utils.logger :as amit]))

(set! *warn-on-reflection* true)


;;; Synchronous Logging

(def ^{:dynamic true
       :doc "Used internally by kits.contextual-logging, to maintain the current logging context."}
  *log-context* {})

(defmacro in-log-context
  "Any calls to contextual-logging/log-message or contextual-logging/log-exception
   will have the surrounding context added"
  [log-context-map & body]
  `(binding [kits.contextual-logging/*log-context* (merge kits.contextual-logging/*log-context* (sort ~log-context-map))]
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

(defn log-message
  "Delegates to amit/log-message, appending the context to the messages"
  [& message-tokens]
  (apply amit/log-message (str (stringify *log-context*) (first message-tokens)) (rest message-tokens)))

(defn log-exception
  "Delegates to amit/log-message, appending the context to the messages"
  ([e additional-message]
     (amit/log-exception e (str (stringify *log-context*) additional-message)))
  ([e]
     (let [context (stringify *log-context*)]
       (if (= "" context)
         (amit/log-exception e)
         (amit/log-exception e context)))))


;;; Async Logging

(def async-logging-queue
  "Holds the Java ArrayBlockingQueue for high throughput, async logging."
  (atom nil))

(def queue-timeout-in-ms
  "ArrayBlockingQueue polling timeout in ms before returning nil"
  (* 2 msc/minute))

(defn- log-message-consumer [_thread-name_ _args_]
  (while true
    (when-let [f (q/fetch @async-logging-queue queue-timeout-in-ms)]
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
  (thread/start-thread-pool thread-count "async-logging-queue-consumer" log-message-consumer))

(defn async-log-message
  "Like `log-message`, but executes logging on a dedicated thread pool."
  [& message-tokens]
  (let [bindings (get-thread-bindings)]
    (q/add @async-logging-queue (fn []
                                  (with-bindings bindings
                                    (apply log-message message-tokens))))))

(defn async-log-exception
  "Like `log-exception`, but executes logging on a dedicated thread pool."
  ([e additional-message]
     (let [bindings (get-thread-bindings)]
       (q/add @async-logging-queue (fn []
                                     (with-bindings bindings
                                       (log-exception e additional-message))))))
  ([e]
     (let [bindings (get-thread-bindings)]
       (q/add @async-logging-queue (fn []
                                     (with-bindings bindings
                                       (log-exception e)))))))
