(ns kits.high-troughput-logging-stress-tests
  (:use
    kits.foundation)
  (:require
    [kits.queues :as q]
    [kits.high-throughput-logging :as l]))


(defn format-entry [host pid tid msg]
  (str host pid tid msg))

(defn foo []
  (let [dir "/tmp/htl"
        prefix "High-Throughput-Logging-Stress-Test"
        shutdown (atom false)
        q (q/create 100000)
        pool (q/start-thread-pool
               1
               "High-Throughput-Logging-Stress-Test-Logger"
               (l/make-log-rotate-loop
                 {:queue q
                  :compute-file-name (l/std-log-file-path-fn dir prefix)
                  :formatter format-entry
                  :io-error-handler prn
                  :shutdown shutdown
                  :conf {:queue-timeout-ms 1
                         :rotate-every-minute 1
                         :rotate-every-bytes 1000
                         :max-unflushed 10
                         :max-elapsed-unflushed-ms 10
                         :root dir
                         :filename-prefix prefix}}))]

    (loop [i 0]
      (if (< i 100000)
        (do
          (when-not (q/add q (str "Hello world " i))
            (println "Unable to add to queue! Dropping message")) 
          (recur (inc i)))
        (reset! shutdown true)))))

;; (try (foo) (catch Exception e (.printStackTrace e)))
