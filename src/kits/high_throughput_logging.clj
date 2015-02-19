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

(defn- next-rotate-time [now rotation-minutes]
  (cal/round-up-ts now rotation-minutes))

(defn- format-log-entry [formatter msg]
  (let [host (runtime/host)
        pid (runtime/process-id)
        tid (runtime/thread-id)]
    (try
      (formatter host pid tid msg)
      (catch Throwable e
        (.printStackTrace e)))))

(defn- create-log-file-writer [file-name-fn ts-ms]
  (let [path (file-name-fn (runtime/thread-id) ts-ms)]
    (FileWriter. ^String path true)))

(defn- rotate-log [create-writer writer io-error-handler rotate-at]
  (io/resilient-close writer io-error-handler)
  (create-writer rotate-at))

(defn make-log-rotate-loop
  "Build a loop to write log entries with a high-throughput rate."
  [{:keys [queue compute-file-name formatter io-error-handler conf]}]
  (fn [thread-name args]
    (let [{:keys [queue-timeout-ms rotate-every-minute max-unflushed max-elapsed-unflushed-ms]} conf
          create-log-file-for (partial create-log-file-writer compute-file-name)
          entry-formatter (partial format-log-entry formatter)]
      (loop [last-flush-at (ms-time)
             unflushed 0
             rotate-at (next-rotate-time last-flush-at rotate-every-minute)
             writer (create-log-file-for rotate-at)]
        (let [msg (q/fetch queue queue-timeout-ms)
              now (ms-time)
              terminate? (= ::terminate msg)
              rotate? (> now rotate-at)
              [rotate-at writer unflushed] (if rotate?
                                             [(next-rotate-time now rotate-every-minute)
                                              (rotate-log create-log-file-for writer io-error-handler rotate-at) 
                                              0]
                                             [rotate-at writer unflushed])
              is-log-msg? (and (not terminate?) (boolean msg)) ; msg is not false or nil (flush heartbeat triggers)
              unflushed (if is-log-msg? 
                          (do
                            (io/resilient-write writer (str (entry-formatter msg) "\n") io-error-handler)
                            (inc unflushed))
                          unflushed)
              flush? (and (pos? unflushed) ; have some unflushed data
                          (or terminate? ; stopping the loop
                              (> (- now last-flush-at) max-elapsed-unflushed-ms) ; unflushed time limit
                              (> unflushed max-unflushed)))] ; unflushed count limit
          (if flush?
            (do
              (io/resilient-flush writer io-error-handler)
              (when-not terminate?
                (recur (ms-time) 0 rotate-at writer)))
            (when-not terminate?
              (recur last-flush-at unflushed rotate-at writer))))))))

(defn stop-log-rotate-loop [queue]
  (q/add queue ::terminate))
