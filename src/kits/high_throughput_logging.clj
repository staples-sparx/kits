(ns kits.high-throughput-logging
  "High throuput logging flushing to disk every N message and/or M milliseconds, whichever comes first"
  (:use
    kits.foundation)
  (:require
    [kits.io :as io]
    [kits.calendar :as cal]
    [kits.runtime :as runtime]
    [kits.queues :as q])
  (:import
    java.io.FileWriter))

(defn std-log-file-path-fn
  "Convenience function which return a function can be used as the
   compute-file-name setting when calling make-log-rotate-loop. Write
   logs under dir-path with a file name starting with prefix. Also
   appends information about when that file will be rotated to simplify
   log shipping and isolates logs by including the thread-id in the
   name."
  [dir-path prefix]
  (fn [thread-id next-rotate-at]
    (str
      dir-path
      "/"
      prefix
      next-rotate-at
      "-"
      (cal/day-at next-rotate-at)
      "-"
      thread-id
      ".log")))

(defn make-log-rotate-loop
  "Build a loop that can be used in a thread pool to log entry with a
  very high-troughput rate. Code is quite ugly but by lazily rotating
  and flushing the writer we achieve very troughput."
  [{:keys [queue compute-file-name formatter io-error-handler conf]}]
  (let [{:keys [queue-timeout-ms
                rotate-every-minute
                max-unflushed
                max-elapsed-unflushed-ms]} conf
        compute-next-rotate-at (fn [now]
                                 (cal/round-up-ts now rotate-every-minute))
        log-file-for (fn ^FileWriter [ts]
                       (let [path (compute-file-name (runtime/thread-id) ts)]
                         (FileWriter. ^String path true)))
        enforce-log-rotation-policy (fn [now rotate-at writer unflushed-msgs]
                                      (if (> now rotate-at)
                                        [(compute-next-rotate-at now)
                                         (do
                                           (io/resilient-close writer io-error-handler)
                                           (log-file-for rotate-at))
                                         0]
                                        [rotate-at writer unflushed-msgs]))
        enforce-flush-policy (fn [unflushed-msgs elapsed-unflushed-ms writer]
                               (if (or
                                     (> unflushed-msgs max-unflushed)
                                     (> elapsed-unflushed-ms max-elapsed-unflushed-ms))
                                 (do
                                   (io/resilient-flush writer io-error-handler)
                                   true)
                                 false))]
    (fn [thread-name args]
      (let [host (runtime/host)
            pid (runtime/process-id)
            tid (runtime/thread-id)
            now (ms-time)
            entry-formatter (fn [msg]
                              (try
                                (formatter host pid tid msg)
                                (catch Throwable e
                                  (.printStackTrace e))))]
                (loop [last-flush-at now
               unflushed-msgs 0
               rotate-at (compute-next-rotate-at now)
               writer (log-file-for rotate-at)]
          (let [msg (q/fetch queue queue-timeout-ms)
                now (ms-time)
                [rotate-at writer unflushed-msgs] (enforce-log-rotation-policy
                                                    now
                                                    rotate-at
                                                    writer
                                                    unflushed-msgs)
                elapsed-unflushed-ms (- now last-flush-at)]
            (if-not msg
              ;;
              ;; Idle, just check whether we should flush and get back
              ;; to business
              (if (and
                    (pos? unflushed-msgs)
                    (> elapsed-unflushed-ms max-elapsed-unflushed-ms))
                (do
                  (io/resilient-flush writer io-error-handler)
                  (recur (ms-time) 0 rotate-at writer))
                (recur last-flush-at unflushed-msgs rotate-at writer))

              ;;
              ;; New log entry, write it and flush only when needed
              ;;
              (do
                (io/resilient-write writer
                  (str (entry-formatter msg) "\n")
                  io-error-handler)
                (if (enforce-flush-policy unflushed-msgs elapsed-unflushed-ms writer)
                  (recur (ms-time) 0 rotate-at writer)
                  (recur last-flush-at (inc unflushed-msgs) rotate-at writer))))))))))
