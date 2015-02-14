(ns kits.queues
  "Wrappers for constructing various java.util.concurrent queues."
  (:refer-clojure :exclude [get peek empty?])
  (:use
    kits.foundation)
  (:require
    [kits.thread :as thread])
  (:import
    java.util.concurrent.ArrayBlockingQueue
    java.util.concurrent.BlockingQueue
    java.util.concurrent.PriorityBlockingQueue
    java.util.concurrent.TimeUnit))

(set! *warn-on-reflection* true)

(defn create
  "Create a new queue that can hold at max 'capacity' items"
  [capacity]
  (ArrayBlockingQueue.
    (int capacity)))

(defn create-with-priority
  "Create a new priority-queue with `comparator` and `capacity` items"
  [comparator capacity]
  (PriorityBlockingQueue. capacity comparator))

(defn add
  "Add a new msg to the queue. Returns false if the msg could not be
   added because the queue is full, true otherwise."
  [q msg]
  (.offer ^BlockingQueue q msg))

(defn fetch
  "Retrieves a message from the queue, waiting if necessary until an
   element becomes available."
  [q timeout-in-ms]
  (.poll ^BlockingQueue q timeout-in-ms TimeUnit/MILLISECONDS))

(defn peek
  "Retrieves, but does not remove, a message from the queue"
  [^BlockingQueue q]
  (.peek q))

(def offer! add)                        ; compatibility alias

(def poll! fetch)                       ; compatibility alias

(defn used [^BlockingQueue q]
  (.size q))

(defn empty? [q]
  (== 0 (used q)))

(defn free [^BlockingQueue q]
  (.remainingCapacity q))


(defn stats
  "Return current stats for the queue"
  [^BlockingQueue q]
  (let [s (used q)
        r (free q)]
    {:total (_+ s r)
     :used s
     :free r}))

(defn start-thread-pool
  "Starts a thread pool with 'thread-count' threads. 'f' is a function
   that constitute the tread loop. Its first argument is the thread
   name, the second argument will be set  to match the 'args' given
   when invoking 'start-thread-pool'"
  [thread-count name-prefix f & args]
  (doall
    (map (fn [thread-num]
           (let [name (str name-prefix thread-num)
                t (Thread. ^Runnable (partial f name args) ^String name)]
            (.start t)
            t))
         (range 0 (int thread-count)))))

(defn join-thread-pool
  "Join all threads in a thread pool."
  ([pool] (join-thread-pool pool nil))
  ([pool timeout-ms]
    (doall
      (map (fn [thread]
             (if timeout-ms
               (.join thread (long timeout-ms))
               (.join thread))
             thread)
           pool))))

