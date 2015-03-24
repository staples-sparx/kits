(ns ^{:doc "clojure.tools.logging logger that forwards to kits.logging.log-async"}
  kits.logging.logger
  (:require
    [clojure.tools.logging :as logging]
    [clojure.tools.logging.impl :as impl]
    [kits.homeless :refer [exception->map]]
    [kits.logging.log-async :as async]))

(defn- log-dict [log-ns throwable message]
  (cond-> {:namespace log-ns}
    (string? message) (assoc :message message)
    (map? message) (merge message)
    throwable (assoc :exception (exception->map throwable))))

(defn- log-level [tools-level]
  (case tools-level
    :trace :TRACE
    :debug :DEBUG
    :info :INFO
    :warn :WARN
    :error :ERROR
    :fatal :ERROR
    :ERROR))

(defn logger [log-ns]
  (reify impl/Logger

    (enabled? [this level] true) ; decision is forwarded to log-async

    (write! [this level throwable message]
      (let [msg (log-dict log-ns throwable message)
            level (log-level level)]
        (async/log level msg)))))

(defn logger-factory [previous-logger-factory]
  (reify impl/LoggerFactory

    (name [this] "kits.logging.logger")

    (get-logger [this logger-ns]
      (logger logger-ns))))

(defn install! []
  (alter-var-root (var logging/*logger-factory*)
                  logger-factory))

