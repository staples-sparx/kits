(ns kits.monitored-seq)


(defmacro ^:private safely [& body]
  `(try
     ~@body
     (catch Exception e#
       (log/error e# "operation in monitored seq failed"))))

(defn- print-seq-progress
  [{:keys [overall-rate current-rate current-index count] :as m}]
  (print
   (str "at element " current-index ",")
   "current rate of" (format "%1.2f" (double current-rate)) "elements/sec,"
   "overall rate of" (format "%1.2f" (double overall-rate)) "elements/sec")
  (when count
    (let [rate (/ (+ current-rate overall-rate) 2)]
      (when (pos? rate)
        (print "," (long (/ (- count current-index) rate)) "seconds remaining"))))
  (println))

(defn- update-seq-progress
  [{:keys [count start-time last-time last-index current-index-atom]}]
  (let [now (System/currentTimeMillis)
        elapsed (- now last-time)
        current-index @current-index-atom
        current-rate (/ (- current-index last-index) (/ elapsed 1000))
        overall-rate (/ (inc current-index) (/ (- now start-time) 1000))]
    (print-seq-progress
     {:overall-rate overall-rate
      :current-rate current-rate
      :count count
      :current-index current-index})
    {:count count
     :start-time start-time
     :last-time now
     :last-index current-index
     :current-index-atom current-index-atom}))

(defn- monitor-seq-progress [period count index complete?]
  (let [now (System/currentTimeMillis)]
    (loop [state {:count count
                  :start-time now
                  :last-time now
                  :last-index 0
                  :current-index-atom index}]
      (Thread/sleep period)
      (if @complete?
        (do
          (println "DONE. Iterated over" @index "elements in" (- (System/currentTimeMillis) now) "milliseconds.")
          (flush))
        (recur (update-seq-progress state))))))

(defn monitored-seq
  ([s]
     (monitored-seq 5000 s))
  ([period s]
     (let [cnt (when (counted? s) (count s))]
       (when (empty? s)
         (println "EMPTY.")
         (flush))
       (letfn [(create-monitored-seq [idx s index complete?]
                 (lazy-seq
                  (when (zero? idx)
                    (future
                      (try
                        (monitor-seq-progress period cnt index complete?)
                        (catch Exception e (.printStackTrace e)))))
                  (if (empty? s)
                    (do
                      (reset! complete? true)
                      nil)
                    (do
                      (swap! index #(max % idx))
                      (cons
                       (first s)
                       (create-monitored-seq (inc idx) (rest s) index complete?))))))]
         (create-monitored-seq 0 s (atom 0) (atom false))))))