(ns ^{:doc "This file provides a simple API over
            kits.logging.log-generator and kits.logging.log-consumer. Use
            start-thread-pool! to start a logger and the familiar logging
            methods (info, warn, error, etc) to write to it asyncronously. If
            you require more control, feel free to use log-generator and
            log-consumer directly, with this ns as an example."}
   kits.logging.log-async
  (:require
   [kits.queues :as q]
   [kits.thread :as t]
   [kits.homeless :as hl]
   [kits.logging.log-consumer :as log-consumer]
   [kits.logging.log-generator :as log-generator]
   [kits.structured-logging :as sl]))

(set! *warn-on-reflection* true)

(defonce log-q (atom nil))

(defn reset-q!
  ([max-msgs] (reset-q! log-q max-msgs))
  ([queue max-msgs] (reset! queue (q/create max-msgs))))

(defn exception
  ([e msg] (exception log-q e msg))
  ([queue e msg] (q/add @queue (assoc msg
                                  :log-level :ERROR
                                  :exception (hl/exception->map e)))))

(defn error
  ([msg] (error log-q msg))
  ([queue msg] (q/add @queue (assoc msg
                               :log-level :ERROR))))

;; TODO: warn should take a throwable but leaving it
;;       for now to avoid breaking the existing sig. -sd
(defn warn
  ([msg] (warn log-q msg))
  ([queue msg] (q/add @queue (assoc msg :log-level :WARN))))

(defn info
  ([msg] (info log-q msg))
  ([queue msg] (q/add @queue (assoc msg :log-level :INFO))))

(defn debug
  ([msg] (debug log-q msg))
  ([queue msg] (q/add @queue (assoc msg :log-level :DEBUG))))

(defn trace
  ([msg] (trace log-q msg))
  ([queue msg] (q/add @queue (assoc msg :log-level :TRACE))))

(defn start-thread-pool!
  "Create a log queue and threadpool for consuming log messages
   and writing them to disk. log-config should have this shape:
   {:root \"/var/log/magrathea\"
    :filename-prefix \"magrathea\"
    :default-context \"magrathea::\"
    :thread-count 1
    :thread-prefix \"Magrathea-Log-\"
    :rotate-every-minute 60
    :max-msg 10000
    :max-unflushed 10000
    :max-elapsed-unflushed-ms 3000
    :queue-timeout-ms 1000}"
  ([log-config] (start-thread-pool! log-q log-config))
  ([queue log-config]
     (reset-q! queue (get log-config :max-msg 10000))
     (t/start-thread-pool
      (:thread-count log-config)
      (:thread-prefix log-config)
      (log-consumer/make-log-rotate-loop
       {:queue @queue
        :compute-file-name log-generator/log-file-path-for
        :formatter (or (:formatter-fn log-config)
                       (partial log-generator/log-formatter
                                (:default-context log-config)))
        :io-error-handler (or (:io-error-handler log-config)
                              (partial log-consumer/stdout))
        :conf log-config}))))
