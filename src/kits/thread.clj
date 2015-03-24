(ns ^{:doc "Thread utility functions"}
  kits.thread
  (:import
    (java.util.concurrent
      RejectedExecutionHandler
      RejectedExecutionException
      ThreadFactory
      ThreadPoolExecutor)))

(set! *warn-on-reflection* true)

(defn start-thread-pool
  "Starts a thread pool with 'thread-count' threads. 'f' is a function
   that constitute the tread loop. Its first argument is the thread
   name, the second argument will be set to match the 'args' given
   when invoking 'start-thread-pool'. Returns a seq of threads."
  [thread-count name-prefix f & args]
  (doall (for [thread-num (range 0 (int thread-count))
               :let [thread-name (str name-prefix thread-num)
                     t (Thread. ^Runnable (partial f thread-name args) ^String thread-name)]]
           (doto t (.start)))))

(defn- safe-thread-join [any-ex timeout-ms thread]
  "Join a thread and catch an InterruptedException into any-ex."
  (try 
    (if (some? timeout-ms)
      (.join ^Thread thread (long timeout-ms))
      (.join ^Thread thread))
    (catch InterruptedException e
      (when (nil? @any-ex)
      (var-set any-ex e))))
  thread)

(defn join-thread-pool
  "Join all threads in a thread pool.
   Throws InterruptedException if any thread was interrupted during the join."
  ([pool] (join-thread-pool pool nil))
  ([pool timeout-ms]
   (with-local-vars [any-ex nil]
     (let [join-thread (partial safe-thread-join any-ex timeout-ms)
           joined-pool (doall (map join-thread pool))]
       (when (some? @any-ex)
         (throw @any-ex))
       joined-pool))))

(defn thread-factory [thread-prefix thread-exception-handler]
  (let [thread-num (atom -1)]
    (reify ThreadFactory
      (newThread [_ runnable]
        (swap! thread-num inc)
        (let [thread-name (str thread-prefix "-" @thread-num)]
          (Thread. (fn []
                     (try
                       (.run ^Runnable runnable)
                       (catch Exception e
                         (thread-exception-handler e)
                         (throw e))))
                   thread-name))))))

(defn ->abort-policy [^String message]
  (reify RejectedExecutionHandler
    (rejectedExecution [_ _runnable _executor]
      (throw (RejectedExecutionException. message)))))

(defn pool-stats [^ThreadPoolExecutor threadpool]
  {:current-pool-size (.getPoolSize threadpool)
   :max-pool-size (.getMaximumPoolSize threadpool)
   :num-active-threads (.getActiveCount threadpool)
   :num-completed-tasks (.getCompletedTaskCount threadpool)
   :num-queued (-> threadpool .getQueue .size)
   :num-remaining (-> threadpool .getQueue .remainingCapacity)})

(defn current-thread-id []
  (.getId (Thread/currentThread)))
