(ns ^{:doc "Wrappers for constructing various java.util.concurrent queues."}
  kits.queues
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
