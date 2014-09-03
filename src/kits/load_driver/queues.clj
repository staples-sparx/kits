(ns kits.load-driver.queues
  (:import (java.util.concurrent LinkedBlockingDeque
                                 TimeUnit)))

(defn create
  "Create a new queue that can hold at max 'capacity' items"
  [capacity]
  (LinkedBlockingDeque. (int capacity)))

(defn add
  "Add a new msg to the queue. Returns false if the msg
  could not be added because the queue is full, true
  otherwise."
  [q msg]
  (.offer ^LinkedBlockingDeque q msg))

(defn add-first
  "Add a new msg to the head of the queue. Returns false if the msg
  could not be added because the queue is full, true
  otherwise."
  [q msg]
  (.offerFirst ^LinkedBlockingDeque q msg))

(defn fetch
  "Retrieves a message from the queue, waiting if necessary until an element becomes available."
  [q timeout-in-ms]
  (.poll ^LinkedBlockingDeque q timeout-in-ms TimeUnit/MILLISECONDS))

(defn used [q]
  (.size ^LinkedBlockingDeque q))

(defn free [q]
  (.remainingCapacity ^LinkedBlockingDeque q))

(defn stats
  "Return current stats for the queue"
  [^LinkedBlockingDeque q]
  (let [s (used q)
        r (free q)]
    {:total (+ s r)
     :used s
     :free r}))

(defn start-thread-pool
  "Starts a thread pool with 'thread-count' threads. 'f' is a function
  that constitute the tread loop. Its first argument is the thread
  name, the second argument will be set  to match the 'args' given
  when invoking 'start-thread-pool'"
  [thread-count name-prefix f & args]
  (doall
   (map #(let [name (str name-prefix %)
               t (Thread. ^Runnable (partial f name args) ^String name)]
           (.start t)
           t)
        (range 0 (int thread-count)))))
