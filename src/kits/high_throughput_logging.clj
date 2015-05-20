(ns kits.high-throughput-logging
  "High throughput logging that can easily sustain 40K of messages/s
   with a single thread.

   The core idea to achieve this level of throughput is to write to disk
   sequentially and batch flushing. We flush to disk only every N message
   and/or M milliseconds, whichever comes first.

   To guarantee the integrity of the log files (no partial writes of an
   entry), we also take care of log rotation in the same loop that is in
   charge of the log entry writing (this way we can ensure that rotation
   can only happen at entry boundaries). Rotation can happen based on
   time or number of bytes written, whichever comes first.

   If std-log-file-path-fn convenience function is used, it employs a
   strategy where the log file name contains the timestamp of when it
   will get rotated (time of the last possible write). This enable a
   simple log shipping strategy outside of the application logic via an
   external program: just look at any file name and only 'ship' it
   if its rotation time is in the past.

   Note: there is a slight shortcoming in that we compute the name of
   the next file based on the next rotation time in milliseconds. If we
   logrotate multiple times whithin the same milliseconds (e.g. because
   we want rotation every 10 bytes), then the file names will all be the
   same within the same millisecond, and the log file will be bigger than
   we expect - but with no data loss, all entries will be there. This is
   quite a corner case, and fixing it would bring complexity that does
   not seem waranteed at this point.

"
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
   log shipping. Also isolates logs by including the thread-id in the
   name."
  [dir-path prefix]
  (fn [thread-id next-rotate-at]
    (str
      dir-path
      "/"
      prefix
      next-rotate-at                ;; Nice for machine parsing
      "-"
      (cal/day-at next-rotate-at)   ;; Nice for humans
      "-"
      thread-id                     ;; Make it safe to use multiple logging threads
      ".log")))

(defn make-log-rotate-loop
  "Build a loop that can be used in a thread pool to log entry with a
   very high-troughput rate. Code is quite ugly but by lazily rotating
   and flushing the writer we achieve very troughput."
  [{:keys [queue compute-file-name formatter io-error-handler shutdown conf]}]
  (let [{:keys [queue-timeout-ms
                rotate-every-minute
                rotate-every-bytes
                max-unflushed
                max-elapsed-unflushed-ms]} conf
                compute-next-rotate-at (fn [now]
                                         (cal/round-up-ts now rotate-every-minute))
                log-file-for (fn ^FileWriter [ts]
                               (let [path (compute-file-name (runtime/thread-id) ts)]
                                 (FileWriter. ^String path true)))

                enforce-log-rotation-policy (fn [^long now ^long rotate-at writer ^long bytes ^long unflushed-msgs
                                                 ]
                                              (if (or
                                                    (> now rotate-at)
                                                    (>= bytes rotate-every-bytes))
                                                (let [_ (io/resilient-close writer io-error-handler)
                                                      rotate-at (compute-next-rotate-at now)
                                                      writer (log-file-for rotate-at)]
                                                  [rotate-at writer 0 0])
                                                [rotate-at writer bytes unflushed-msgs]))

                enforce-flush-policy (fn [^long unflushed-msgs ^long elapsed-unflushed-ms writer]
                                       (if (or
                                             (> unflushed-msgs max-unflushed)
                                             (and
                                               (pos? unflushed-msgs)
                                               (> elapsed-unflushed-ms max-elapsed-unflushed-ms)))
                                         (do
                                           (io/resilient-flush writer io-error-handler)
                                           true)
                                         false))]
    (fn [thread-name args]
      (let [host (runtime/host)
            pid (runtime/process-id)
            tid (runtime/thread-id)
            now (ms-time)
            resilient-formatter (fn [msg]
                                  (try
                                    (formatter host pid tid msg)
                                    (catch Throwable e
                                      (.printStackTrace e))))]
        (loop [rotate-at ^long (compute-next-rotate-at now)
               writer (log-file-for rotate-at)
               bytes 0
               last-flush-at now
               unflushed-msgs 0]
          (let [msg (q/fetch queue queue-timeout-ms)
                now (ms-time)
                ;; Enforce log rotation before doing anything else
                [rotate-at writer bytes unflushed-msgs] (enforce-log-rotation-policy
                                                          now
                                                          rotate-at
                                                          writer
                                                          bytes
                                                          unflushed-msgs)
                elapsed-unflushed-ms (- now last-flush-at)]
            (if-not msg
              ;; Idle, just check whether it's time to flush and get
              ;; back to business... unless shutdown has been
              ;; requested, we are actually at a good point for that.
              (if (enforce-flush-policy unflushed-msgs elapsed-unflushed-ms writer)
                (do
                  (io/resilient-flush writer io-error-handler)
                  (when-not @shutdown
                    (recur rotate-at writer bytes (ms-time) 0)))
                (when-not @shutdown
                  (recur rotate-at writer bytes last-flush-at unflushed-msgs)))

              ;; New log entry, write it and flush only when needed
              (let [line (str (resilient-formatter msg) "\n")
                    new-bytes (alength (.getBytes line "UTF-8"))
                    bytes (+ bytes new-bytes)]
                (io/resilient-write writer line io-error-handler)
                (if (enforce-flush-policy unflushed-msgs elapsed-unflushed-ms writer)
                  (recur rotate-at writer bytes (ms-time) 0)
                  (recur rotate-at writer bytes last-flush-at (inc unflushed-msgs)))))))))))
