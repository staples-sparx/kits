(ns kits.queues
  (:refer-clojure :exclude [get peek])
  (:import
   [java.util.concurrent
    TimeUnit BlockingQueue ArrayBlockingQueue PriorityBlockingQueue]))

(defn make-basic-queue
  "Create a new queue that can hold at max 'capacity' items"
  [& [capacity]]
  (let [capacity (or capacity 10)]
    (ArrayBlockingQueue. (int capacity))))

(def create make-basic-queue)           ; compatibility alias

(defn make-priority-queue
  "Create a new priority-queue with `comparator` and `capacity` items"
  [comparator & [capacity]]
  (let [capacity (or capacity 10)]
    (PriorityBlockingQueue. capacity comparator)))

(defn offer!
  "Add a new msg to the queue. Returns false if the msg could not be
   added because the queue is full, true otherwise."
  [^BlockingQueue q msg]
  (.offer q msg))

(def add offer!)                        ; compatibility alias

(defn poll!
  "Retrieves a message from the queue, waiting if necessary until an
   element becomes available."
  [^BlockingQueue q & [timeout-in-ms]]
  (.poll q (or timeout-in-ms 0) TimeUnit/MILLISECONDS))

(def fetch poll!)                       ; compatibility alias

(defn peek
  "Retrieves, but does not remove, a message from the queue"
  [^BlockingQueue q]
  (.peek q))

(defn used [^BlockingQueue q]
  (.size q))

(defn free [^BlockingQueue q]
  (.remainingCapacity q))

(defn stats
  "Return current stats for the queue"
  [^BlockingQueue q]
  (let [s (used q)
        r (free q)]
    {:total (+ s r)
     :used s
     :free r}))

(defn start-thread-pool
  "Starts a thread pool with 'thread-count' threads. 'f' is a function
   that constitute the tread loop. Its first argument is the thread
   name, the second argument will be set to match the 'args' given
   when invoking 'start-thread-pool'"
  [thread-count name-prefix f & args]
  (doall
   (map #(let [^String name (str name-prefix %)
               t (Thread. ^Runnable (partial f name args) name)]
           (.start t)
           t)
        (range 0 (int thread-count)))))
