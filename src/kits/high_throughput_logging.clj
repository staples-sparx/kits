(ns kits.high-throughput-logging
  "High throuput logging flushing to disk every N message and M milliseconds"
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
  (let [{:keys [queue-timeout-ms rotate-every-minute max-unflushed max-elapsed-unflushed-ms]} conf
        compute-next-rotate-at (fn [now]
                                 (cal/round-up-ts now rotate-every-minute))
        log-file-for (fn [ts]
                       (let [path (compute-file-name (runtime/thread-id) ts)]
                         {:path path
                          :writer (FileWriter. ^String path true)}))]
    (fn [thread-name args]
      (let [now (ms-time)
            rotate-at (compute-next-rotate-at now)
            {:keys [path writer]}  (log-file-for rotate-at)
            host (runtime/host)
            pid (runtime/process-id)
            tid (runtime/thread-id)
            entry-formatter (fn [msg]
                              (try
                                (formatter host pid tid msg)
                                (catch Throwable e
                                  (.printStackTrace e))))]
        ;; (log/info "Starting thread writing to " path)
        (loop [last-flush-at now
               unflushed 0
               rotate-at rotate-at
               writer writer]
          ;; (log/debug thread-name " | Fetching a message...")
          (let [msg (q/fetch queue queue-timeout-ms)
                now (ms-time)]
            ;; Check whether we should rotate the logs
            (let [rotate? (> now rotate-at)
                  rotate-at (if-not rotate?
                              rotate-at
                              (compute-next-rotate-at now))
                  writer (if-not rotate?
                           writer
                           (do
                             (io/resilient-close writer io-error-handler)
                             (:writer (log-file-for rotate-at))))]
              (if-not msg
                ;; Check whether we should flush and get back to business
                (if (and
                      (pos? unflushed)
                      (> (- now last-flush-at) max-elapsed-unflushed-ms))
                  (do
                    ;; (log/debug "Flush inactive")
                    (io/resilient-flush ^FileWriter writer io-error-handler)
                    (recur (ms-time) 0 rotate-at writer))
                  (recur last-flush-at unflushed rotate-at writer))

                ;; Write log entry, flushing lazily
                (do
                  ;; (log/debug "Got msg" msg)
                  (io/resilient-write writer (str (entry-formatter msg) "\n") io-error-handler)

                  (if (or (> (- now last-flush-at) max-elapsed-unflushed-ms)
                        (> unflushed max-unflushed))
                    (do
                      ;; (log/debug "Flush")
                      (io/resilient-flush writer io-error-handler)
                      (recur (ms-time) 0 rotate-at writer))
                    (recur last-flush-at (inc unflushed) rotate-at writer)))))))))))
