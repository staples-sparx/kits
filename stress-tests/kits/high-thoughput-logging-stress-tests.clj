(ns kits.high-troughput-logging-stress-tests
  (:use
    kits.foundation)
  (:require
    [kits.io :as io]
    [kits.queues :as q]
    [kits.high-throughput-logging :as l]))


(defn format-entry [host pid tid msg]
  (str host pid tid msg))

(defn foo []
  (let [log-dir "/tmp/htl"]
    (io/mkdirs log-dir)
    (let [prefix "High-Throughput-Logging-Stress-Test"
          shutdown-requested? (atom false)
          q (q/create 100000)
          pool (q/start-thread-pool
                 1
                 "High-Throughput-Logging-Stress-Test-Logger"
                 (l/make-log-rotate-loop
                   {:queue q
                    :compute-file-name (l/std-log-file-path-fn log-dir prefix)
                    :formatter format-entry
                    :io-error-handler prn
                    :shutdown-requested? shutdown-requested?
                    :conf {:queue-timeout-ms 1
                           :rotate-every-minute 1
                           :rotate-every-bytes 100000
                           :max-unflushed 100
                           :max-elapsed-unflushed-ms 100
                           :root log-dir
                           :filename-prefix prefix}}))]

      (Thread/sleep 100) ;; Give a chance for the threadpool to start
      (loop [i 0]
        (if (>= i 100)
          (reset! shutdown-requested? true)
          (do
            (if-not (q/add q (str "Hello world " i))
              (println "Ooops dropped a message")
              (println i)
              )
            (recur (inc i))))))))

;; (try (foo) (catch Exception e (.printStackTrace e)))
;; (foo)
