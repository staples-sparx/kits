(ns ^{:doc "Internal namespace. This spawns agents that writes log messages to file and rotates them"}
  kits.logging.log-consumer
  (:require
    [kits.runtime :as runtime]
    [kits.queues :as q]
    [kits.logging.log-generator :as log])
  (:import
    (java.util Calendar TimeZone)
    (java.io FileWriter Writer IOException)))

(set! *warn-on-reflection* true)

(def utc-tz (TimeZone/getTimeZone "UTC"))
(defmacro _+ [a b] `(unchecked-add (long ~a) (long ~b)))
(defmacro _- [a b] `(unchecked-subtract (long ~a) (long ~b)))
(defmacro _* [a b] `(unchecked-multiply (long ~a) (long ~b)))

(defn ms-time
  " Returns number of milli-seconds since the epoch"
  []
  (System/currentTimeMillis))

(defn resilient-close
  "Close a writer ensuring that no exception can be triggered and we do not write anything to disk."
  [^Writer writer error-callback]
  (when writer
    (try
      (.close writer)
      (catch Exception e
        (error-callback e)))))

(defn resilient-flush
  "Flush 'writer', but ignore any java.io.Exception. Useful to avoid
  killing a logging loop when file system is full for instance."
  [^Writer writer error-callback]
  (try
    (.flush writer)
    (catch IOException e
      (error-callback e))))

(defn resilient-write
  "Write 'data' using 'writer', but ignore any java.io.Exception. Useful
  to avoid killing a logging loop when file system is full for
  instance."
  [^Writer writer ^String data error-callback]
  (try
    (.write writer data)
    (catch IOException e
      (error-callback e))))

(defn utc-cal-at
  "Returns a UTC Java calendar set at a specific point-of-time (timestamp in ms)"
  [ts]
  (doto (Calendar/getInstance)
    (.setTimeZone utc-tz)
    (.setTimeInMillis ts)))

(defn round-up-ts
  "Return a timestamp rounded up to the next 'n' minutes"
  [ts n-minutes]
  (let [c ^Calendar(utc-cal-at ts)
        min (.get c Calendar/MINUTE)
        n (Math/floor (/ min n-minutes))
        rounded-down (.getTimeInMillis
                       (doto ^Calendar c
                         (.set Calendar/MINUTE (* n n-minutes))))]
    (_+ rounded-down (_* n-minutes 60000))))

(defn stdout [log-line]
  (print log-line)
  (flush))

(defn make-log-rotate-loop
  "Build a loop that can be used in a thread pool to log entry with a
  very high-troughput rate. Code is quite ugly but by lazily rotating
  and flushing the writer we achieve very troughput."
  [{:keys [queue compute-file-name formatter io-error-handler conf]}]
  (let [{:keys [queue-timeout-ms rotate-every-minute max-unflushed max-elapsed-unflushed-ms]} conf
        compute-next-rotate-at (fn [now] (round-up-ts now rotate-every-minute))
        log-file-for (fn [ts]
                       (let [path (compute-file-name conf (runtime/thread-id) ts)]
                         {:path path
                          :writer (FileWriter. ^String path true)}))]
    (fn [thread-name args]
      (try
        (let [now (ms-time)
              rotate-at (compute-next-rotate-at now)
              {:keys [path writer]}  (log-file-for rotate-at)]
          (stdout (log/info "log-consumer::make-log-rotate-loop"
                                (str "Log file: " path)))
          (stdout (log/info "log-consumer::make-log-rotate-loop"
                                 "Starting fetch loop for logging queue..."))
          (loop [last-flush-at now
                 unflushed 0
                 rotate-at rotate-at
                 writer writer]
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
                               (resilient-close writer io-error-handler)
                               (:writer (log-file-for rotate-at))))]
                (if-not msg
                  ;; Check whether we should flush and get back to business
                  (if (and
                       (pos? unflushed)
                       (> (- now last-flush-at) max-elapsed-unflushed-ms))
                    (do
                      (stdout  (log/info "log-consumer::make-log-rotate-loop"
                                             "Flush inactive"))
                      (resilient-flush ^FileWriter writer io-error-handler)
                      (recur (ms-time) 0 rotate-at writer))
                    (recur last-flush-at unflushed rotate-at writer))

                  ;; Write log entry, flushing lazily
                  (do
                    (resilient-write writer (formatter msg) io-error-handler)
                    (if (or (> (- now last-flush-at) max-elapsed-unflushed-ms)
                            (> unflushed max-unflushed))
                      (do
                        (stdout (log/info "log-consumer::make-log-rotate-loop" "Flush"))
                        (resilient-flush writer io-error-handler)
                        (recur (ms-time) 0 rotate-at writer))
                      (recur last-flush-at (inc unflushed) rotate-at writer))))))))
        (catch Exception e
          (stdout (log/error
                       "log-consumer::make-log-rotate-loop" "Exception in logging" e)))))))
