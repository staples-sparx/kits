(ns kits.load-driver.rate)

(defonce run-time (atom nil))

(defn reset-state! []
  (reset! run-time nil))

(defn record-runtimes [start end]
  (reset! run-time {:start-time (or (:start-time @run-time) start)
                    :end-time end}))

(defn total-elapsed-ns []
  (- (:end-time @run-time) (:start-time @run-time)))

(defn total-elapsed-secs []
  (/ (total-elapsed-ns) 1000 1000 1000))

(defn req-per-second [total-requests errors]
  (float  (/ (- total-requests (count errors))
             (total-elapsed-secs))))

(defn req-per-second-str [total-requests errors]
  (cond
   (= total-requests errors)
   "No sessions were processed; all were errors."

   (empty? @run-time)
   "No run-time information collected."

   :else
   (let [rps (req-per-second total-requests errors)]
     (format (str "Sessions's HTTP per second: %.2f (%s sessions in %.2f sec)\n"
                  "   Millis per session :     %.2f ms/session\n"
                  "Throughput is the (sessions per second * calls per session); sleep time is ignored")
             rps
             total-requests
             (float (total-elapsed-secs))
             (double (* 1000 (/ 1 rps)))))))
