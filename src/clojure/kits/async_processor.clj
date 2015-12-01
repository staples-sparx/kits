(ns kits.async-processor
  (:require [kits.queues :as q]
            [kits.thread :as t])
  (:import [java.util ArrayList]
           [java.util.concurrent BlockingQueue Executors
            ExecutorService ScheduledExecutorService TimeUnit]))

(set! *warn-on-reflection* true)

(def ^:private process-q (atom nil))
(def ^:private process-pool (atom nil))

(defn- counted-thread-factory
  "Create a ThreadFactory that maintains a counter for naming Threads.
  name-format specifies thread names - use %d to include counter
  optional daemon is a flag for whether threads are daemons or not"
  ([name-format]
   (counted-thread-factory name-format false))
  ([name-format daemon]
   (let [counter (atom 0)]
     (reify
       ThreadFactory
       (newThread [this runnable]
         (doto (Thread. runnable)
           (.setName (format name-format (swap! counter inc)))
           (.setDaemon daemon)))))))

(def ^:private ^ThreadFactory daemon-thread-factory
  (counted-thread-factory "trillian-executor-%d"))

(defn create-scheduler [pool-size]
  (Executors/newScheduledThreadPool pool-size daemon-thread-factory))

(def ^:private scheduler
  (concurrent/create-scheduler 1))

(defn create-process-pool [n name]
  (swap! process-pool assoc name (Executors/newFixedThreadPool n)))

(defn submit-to-process-pool [name f]
  (.execute ^ExecutorService (@process-pool name) f))

(defn- create-queue [n name]
  (swap! process-q assoc name (q/create n)))

(defn- try-fn [f]
  (try
    (f)
    (catch Exception e
      e)))

(defn- sleep-interval [args]
  (or (:sleep-interval args) 3000))

(defn- with-error-handling
  [item {:keys [exception-handler try-times final-handler] :as args}]
  (loop [i 1]
    (when-let [ex (try-fn
                   #(exception-handler item args))]
      (when (instance? Exception ex)
        (if (= i try-times)
          (final-handler ex item args)
          (recur (inc i)))))))

(defn- with-process-loop [{:keys [process-fn name exception-logger] :as args}]
  (while true
    (when-let [msg (q/poll! (@process-q name)
                            (sleep-interval args))]
      (try
        (process-fn msg)
        (catch Exception e
          (submit-to-process-pool name #(with-error-handling msg args))
          (exception-logger e {:thread name
                               :queue-message msg}))))))

(defn- drain-queue! [^BlockingQueue q ^long size]
  (let [l (ArrayList. size)]
    (.drainTo q l)
    (seq l)))

(defn- schedule-batch-process [f interval]
  (doto ^ScheduledExecutorService scheduler
    (.scheduleWithFixedDelay f 0 interval TimeUnit/MILLISECONDS)))

(defn- setup-batch-process [{:keys [process-fn min-batch-count
                                    name thread-count exception-logger]
                             :as args}]
  (schedule-batch-process
   (fn []
     (let [queue (@process-q name)]
       (when-let [msgs (locking queue
                         (when (>= (-> queue q/stats :used)
                                   min-batch-count)
                           (drain-queue! queue min-batch-count)))]
         (try
           (process-fn msgs)
           (catch Exception e
             (submit-to-process-pool name #(with-error-handling msgs args))
             (exception-logger e {:thread name
                                  :queue-messages msgs}))))))
   (sleep-interval args)))

(defn enqueue
  "Attempts to queue a request - returnes false if the queue is full."
  [name msg]
  (q/add (@process-q name) msg))

(defn q-empty? [qname]
  (q/empty? (@process-q qname)))

(defn q-percent-free [qname]
  (let [{:keys [free total]} (q/stats (@process-q qname))]
    (* 100 (/ free total))))

(defn create-and-start-thread-pool
  "Create a set of thread pools to process messages that come into a queue.

  :name - The name associated with this set of thread pools.

  :process-fn - The function that processes normal messages. This function must
  accept any message/messages that comes through the queue. The return value is never
  looked at, so if the process-fn is not successful and you want to retry it,
  you must raise an exception.

  :exception-handler - The function that processes messages received by raising an
  exception in process-fn. This function must accept a message/messages and a map of
  options. The map may include :try-times, the number of times to try without
  success in error-handler. The default is 3 tries.

  :final-handler - The last function called in a series of errors with a
  message. If a message/messages is unsuccessfully tried up to :try-times, this function
  should be used for alerting, notification and any other means of handling the
  system. It must accept the last exception, a message/messages and the map of args
  originally passed into create-and-start-thread-pool.

  :min-batch-count - Optional, pass if batch-processing mode should be used

  :queue-count - Size of queue. Should be greater then min-batch-count for batch mode

  :thread-count - Number of threads.

  :sleep-interval - Number of milliseconds to sleep between queue polling.
  Defaults to 3000.

  :info-logger - Logging method that logs information entries - takes a map.

  :exception-logger - Logging method for exceptions - takes a map."

  [{:keys [process-fn exception-handler name min-batch-count
           queue-count thread-count info-logger]
    :as args}]
  {:pre [process-fn exception-handler name queue-count thread-count info-logger exception-logger]}
  (info-logger {:tags [name]
                :message (str "Starting " name " async workers.")
                :batching (if min-batch-count min-batch-count "No batching")
                :queue-count queue-count
                :thread-count thread-count})
  (create-queue queue-count name)
  (create-process-pool 1 name)
  (if min-batch-count
    (setup-batch-process args)
    (t/start-thread-pool
     thread-count name (fn [& _] (with-process-loop args))))
  (info-logger {:tags [name]
                :message (str "Started " name " async workers.")}))
