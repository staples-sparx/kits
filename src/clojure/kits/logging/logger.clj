(ns ^{:doc "Structured logging implemenation of Logger for clojure.tools.logging"}
  kits.logging.logger
  (:require
    [clojure.tools.logging :as logging]
    [clojure.tools.logging.impl :as impl]
    [kits.homeless :refer [exception->map]]
    [kits.structured-logging :as s-log]))

(defn- log-dict [log-ns throwable message]
  (cond-> {:namespace log-ns
           :message message}
    throwable (assoc :exception (exception->map throwable))))

(defn- log-tags [throwable]
  (if throwable
    [:tools.logging :exception]
    [:tools.logging]))

(defn logger [cfg local-name log-ns]
  (reify impl/Logger

    (enabled? [this level] true) ; decision is forwarded to structured-log*

    (write! [this level throwable message]
      (s-log/structured-log* cfg local-name level (log-tags throwable) 
                             (log-dict log-ns throwable message)))))

(defn logger-factory [previous-logger-factory cfg local-name]
  (reify impl/LoggerFactory

    (name [this] "kits.logging.logger")

    (get-logger [this logger-ns]
      (logger cfg local-name logger-ns))))

(defn install! [cfg local-name]
  (alter-var-root (var logging/*logger-factory*)
                  logger-factory cfg local-name))

