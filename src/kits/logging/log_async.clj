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
   [kits.homeless :refer [exception->map]]
   [kits.logging.log-consumer :as log-consumer]
   [kits.logging.log-generator :as log-generator]))

(set! *warn-on-reflection* true)

(defonce ^:private log-q (atom nil))

(defn reset-q!
  ([max-msgs] (reset-q! log-q max-msgs))
  ([queue-atom max-msgs] (reset! queue-atom (q/create max-msgs))))

(defn log
  ([level msg] (log @log-q level msg))
  ([queue level msg]
   (q/add queue (assoc msg :log-level level))))

(defn exception
  ([e msg] (exception @log-q e msg))
  ([queue e msg] (log queue :ERROR (merge msg (exception->map e)))))

(defn error
  ([msg] (error @log-q msg))
  ([queue msg] (log queue :ERROR msg)))

;; TODO: warn should take a throwable but leaving it
;;       for now to avoid breaking the existing sig. -sd
(defn warn
  ([msg] (warn @log-q msg))
  ([queue msg] (log queue :WARN msg)))

(defn info
  ([msg] (info @log-q msg))
  ([queue msg] (log queue :INFO msg)))

(defn debug
  ([msg] (debug @log-q msg))
  ([queue msg] (log queue :DEBUG msg)))

(defn trace
  ([msg] (trace @log-q msg))
  ([queue msg] (log queue :TRACE msg)))

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
    :queue-timeout-ms 1000}
   Returns a thread pool."
  ([log-config] (start-thread-pool! log-q log-config))
  ([queue-atom log-config]
     (reset-q! queue-atom (get log-config :max-msg 10000))
     (t/start-thread-pool
      (:thread-count log-config)
      (:thread-prefix log-config)
      (log-consumer/make-log-rotate-loop
       {:queue @queue-atom
        :compute-file-name (partial log-generator/log-file-path-for log-config)
        :formatter (or (:formatter-fn log-config)
                       (partial log-generator/json-log-formatter
                                {:context (:default-context log-config)}))
        :io-error-handler (or (:io-error-handler log-config)
                              (partial log-consumer/stdout))
        :conf log-config}))))

(defn stop-thread-pool!
  "Stop a log thread pool."
  ([pool timeout-ms] (stop-thread-pool! log-q pool timeout-ms)) 
  ([queue-atom pool timeout-ms]
   (log-consumer/stop-log-rotate-loop @queue-atom (count pool) timeout-ms)
   (t/join-thread-pool pool timeout-ms)))
