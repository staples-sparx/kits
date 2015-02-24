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
   when invoking 'start-thread-pool'"
  [thread-count name-prefix f & args]
  (doseq [thread-num (range 0 (int thread-count))
          :let [^String name (str name-prefix thread-num)
                t (Thread. ^Runnable (partial f name args) name)]]
    (.start t)
    t))

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
