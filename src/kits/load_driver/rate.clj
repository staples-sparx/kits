(ns kits.load-driver.rate
  (:require [kits.seq :as seq]))

(defonce step-transit-times-atom (atom []))

(defn reset-state! []
  (reset! step-transit-times-atom []))

(defn record-step-transit-time [start end]
  (swap! step-transit-times-atom conj {:start-time start
                                       :end-time end}))

(defn- elapsed-ns [{:keys [start-time end-time]}]
  (- end-time start-time))

(defn- start-to-end-ns [transit-times]
  (let [earliest (:start-time (seq/min-by :start-time transit-times))
        latest (:end-time (seq/max-by :end-time transit-times))]
    (- latest earliest)))

(defn- ns->s [ns]
  (float (/ ns 1000 1000 1000)))

(defn- ms->ns [ms]
  (* ms 1000000))

(defn- ns->ms [ns]
  (float (/ ns 1000 1000)))

(defn- start-to-end-time-in-secs [transit-times]
  (ns->s (start-to-end-ns transit-times)))

(defn- steps-per-second-of-run [transit-times]
  (float (/ (count transit-times)
            (start-to-end-time-in-secs transit-times))))

(defn- total-elapsed-ns [transit-times]
  (->> transit-times
       (map elapsed-ns)
       (apply +)))

(defn- millis-per-step [transit-times]
  (float
   (/ (ns->ms (total-elapsed-ns transit-times))
      (count transit-times))))

(defn- steps-per-sec [transit-times]
  (float
   (/ (count transit-times)
      (ns->s (total-elapsed-ns transit-times)))))

(defn- rate-report [transit-times errors]
  (let [num-steps (count transit-times)]
    (cond
     (= num-steps (count errors))
     {:error "No sessions were processed; all were errors."}

     (= 0 num-steps)
     {:error "No steps recorded."}

     :else
     {:steps-per-sec-of-run (steps-per-second-of-run transit-times)
      :num-steps num-steps
      :start-to-end-time-in-secs (start-to-end-time-in-secs transit-times)
      :millis-per-step (millis-per-step transit-times)
      :steps-per-sec (steps-per-sec transit-times)})))

(defn rate-report-str [errors]
  (let [report (rate-report @step-transit-times-atom errors)]
    (or (:error report)
        (format
         (str "Steps per sec of run: %.2f, %s steps in %.2f total secs\n"
              "                      (includes all time from beginning of run to end)\n"
              "Millis per step: %.2f ms/step\n"
              "                 (only includes time spent executing steps)\n"
              "Steps per second: %.2f steps/sec \n"
              "                  (only includes time spent executing steps)\n")
         (:steps-per-sec-of-run report)
         (:num-steps report)
         (:start-to-end-time-in-secs report)
         (:millis-per-step report)
         (:steps-per-sec report)))))

(comment
  (use 'kits.timestamp)
  (def x [{:start-time (ms->ns (->timestamp "2014-01-01 12:00:00"))
           :end-time (ms->ns (->timestamp "2014-01-01 12:00:01"))}
          {:start-time (ms->ns (->timestamp "2014-01-01 12:00:00"))
           :end-time (ms->ns (->timestamp "2014-01-01 12:00:01"))}
          {:start-time (ms->ns (->timestamp "2014-01-01 12:00:01"))
           :end-time (ms->ns (->timestamp "2014-01-01 12:00:02"))}])

  (reset! step-transit-times-atom x)

  (print (rate-report-str []))

  (print "Total time: 1.50, 3 steps in 2.00 total secs\n            (includes all time from beginning of run to end)\nMillis per step: 1000000000.00 ms/step\n                 (only includes time spent executing steps)\nSteps per second: 1.00 steps/sec \n                  (only includes time spent executing steps)\n")

  [(steps-per-second x)
   (total-elapsed-ns x)
   (ns->s (total-elapsed-ns x))]
  )
