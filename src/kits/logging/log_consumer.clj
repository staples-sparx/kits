(ns ^{:doc "Internal namespace. This spawns agents that writes log messages to file and rotates them"}
  kits.logging.log-consumer
  (:require
    [kits.io :as io]
    [kits.calendar :as cal]
    [kits.runtime :as runtime]
    [kits.queues :as q]
    [kits.timestamp :as ts])
  (:import
    java.io.FileWriter))

(def ^:private open-file-suffix ".open")

(defn stdout [log-line]
  (println log-line)
  (flush))

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

(defn- formatter-with-context [formatter]
  (let [host (runtime/host)
        pid (runtime/process-id)
        tid (runtime/thread-id)
        context {:host host
                 :pid pid
                 :tid tid}]
    (fn [msg]
      (try
        (str (formatter context msg))
        (catch Throwable e
          (.printStackTrace e))))))

(defn- create-log-file [file-name-fn ts-ms]
  (let [path (str (file-name-fn (runtime/thread-id) ts-ms))
        open-path (str path open-file-suffix)]
    {:open-path open-path
     :dest-path path
     :writer (FileWriter. ^String open-path true)}))

(defn- close-log [log-file io-error-handler]
  (io/resilient-close (:writer log-file) io-error-handler)
  (io/resilient-move (:open-path log-file) (:dest-path log-file) io-error-handler))

(defn- rotate-log [new-log log-file io-error-handler rotate-at]
  (close-log log-file io-error-handler)
  (new-log rotate-at))

(defn make-log-rotate-loop
  "Build a loop to write log entries with a high-throughput rate."
  [{:keys [queue compute-file-name formatter io-error-handler conf]}]
  (fn [thread-name args]
    (let [{:keys [queue-timeout-ms rotate-every-minute max-unflushed max-elapsed-unflushed-ms]} conf
          create-log-file-for (partial create-log-file compute-file-name)
          format-log-msg (formatter-with-context formatter)]
      (loop [last-flush-at (ts/now)
             unflushed 0
             rotate-at (next-rotate-time last-flush-at rotate-every-minute)
             log-file (create-log-file-for rotate-at)
             terminate-ready? false]
        (let [msg (q/fetch queue queue-timeout-ms)
              now (ts/now)
              trigger-terminate? (= ::terminate msg)
              rotate? (> now rotate-at)
              [rotate-at log-file unflushed] (if rotate?
                                               [(next-rotate-time now rotate-every-minute)
                                                (rotate-log create-log-file-for log-file io-error-handler rotate-at) 
                                                0]
                                               [rotate-at log-file unflushed])
              is-log-msg? (and (not trigger-terminate?) (some? msg))
              unflushed (if is-log-msg? 
                          (do
                            (io/resilient-write (:writer log-file)
                                                (format-log-msg msg)
                                                io-error-handler)
                            (inc unflushed))
                          unflushed)
              flush? (and (pos? unflushed) ; have some unflushed data
                          (or (> (- now last-flush-at) max-elapsed-unflushed-ms) ; unflushed time limit
                              (> unflushed max-unflushed)))] ; unflushed count limit
          (if (and terminate-ready? (nil? msg)) ; close iff terminate has been signaled and the queue timed out
            (do
              (close-log log-file io-error-handler)
              (locking queue
                ; signal that we have finished consuming the queue.
                (.notify queue))
              nil)
            (let [terminate-ready? (or terminate-ready? trigger-terminate?)]
              (if flush?
                (do
                  (io/resilient-flush (:writer log-file) io-error-handler)
                  (recur (ts/now) 0 rotate-at log-file terminate-ready?))
                (recur last-flush-at unflushed rotate-at log-file terminate-ready?)))))))))

(defn stop-log-rotate-loop
  "Stop a log rotate loop, blocking until the log writing has finished"
  [queue num-threads timeout-ms]
  {:pre [(integer? timeout-ms)]}
  ; stop each thread
  (dotimes [n num-threads]
    (q/add queue ::terminate)
    (locking queue
      (.wait queue (long timeout-ms))))
  nil)
