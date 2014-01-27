(ns kits.thread
  "Thread utility functions")


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
