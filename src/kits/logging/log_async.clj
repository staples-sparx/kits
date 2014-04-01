(ns kits.logging.async
  "This file provides a simple API over kits.logging.log-generator and
   kits.logging.log-consumer. Use start-thread-pool! to start a logger
   and the familiar logging methods (info, warn, error, etc) to write
   to it asyncronously. If you require more control, feel free to use
   log-generator and log-consumer directly, with this ns as an example."
  (:require
   [kits.queues :as q]
   [kits.thread :as t]
   [kits.logging.log-consumer :as log-consumer]
   [kits.logging.log-generator :as log-generator]))

(defonce log-q (atom nil))

(defn reset-q! [max-msgs]
  (reset! log-q (q/create max-msgs)))

(defn exception [e msg]
  (q/add @log-q (assoc msg :exception e
                      "log-level" :ERROR)))

(defn error [msg]
  (q/add @log-q (assoc msg
                 "log-level" :ERROR)))

;; TODO: warn should take a throwable but leaving it
;;       for now to avoid breaking the existing sig. -sd
(defn warn [msg]
  (q/add @log-q (assoc msg :log-level :WARN)))

(defn info [msg]
  (q/add @log-q (assoc msg :log-level :INFO)))

(defn debug [msg]
  (q/add @log-q (assoc msg :log-level :DEBUG)))

(defn trace [msg]
  (q/add @log-q (assoc msg :log-level :TRACE)))

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
  [log-config & io-error-handler]
  (reset-q! (get log-config :max-msg 10000))
  (let [out *out*]
    (t/start-thread-pool
     (:thread-count log-config)
     (:thread-prefix log-config)
     (log-consumer/make-log-rotate-loop
      {:queue @log-q
       :compute-file-name log-generator/log-file-path-for
       :formatter (partial log-generator/log-formatter
                           (:default-context log-config))
       :io-error-handler (or io-error-handler (partial log-generator/fallback-print out))
       :conf log-config}))))
