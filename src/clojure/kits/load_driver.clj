(ns ^{:doc "Driver for load testing"}
  kits.load-driver
  (:require [kits.load-driver.histograms :as h]
            [kits.load-driver.rate :as rate]
            [kits.thread :as thread])
  (:import (java.util.concurrent LinkedBlockingQueue
                                 TimeUnit)))

;;                       _ Workers --
;;  Generator -> | Q| <-|             --> | Q | <-- Metrics
;;                      \_ Workers --
;;

(def histogram-ceiling (* 5 1000 1000 1000))

(defonce ^LinkedBlockingQueue session-q (LinkedBlockingQueue.))
(defonce ^LinkedBlockingQueue response-q (LinkedBlockingQueue.))
(defonce errors (atom []))
(defonce clear-histograms? (atom false))
(defonce stop-workers? (atom false))
(defonce num-msgs-processed (atom 0))

(defn- non-timeout-errors [errs]
  (->> errs
       (remove #(= "timeout" (:error %)))
       (remove #(nil? (:exception %)))
       (mapv #(.getMessage ^Throwable (:exception %)))))

(defn- error-counts [errs]
  {:all-errors (count errs)
   :timeouts (count (filter #(= "timeout" (:error %)) errs))
   :server (count (filter #(= "server" (:error %)) errs))})

(defn- print-histograms [step-title-fn step-name->histogram response-num]
  (when (zero? (mod response-num 10))
    ;; (println "\n" response-num " api calls so far: ")
    (println "\n===================================================================")
    (doseq [[step-name histogram] step-name->histogram
            :when (not (zero? (h/num-samples histogram)))]
      (let [h (h/collect histogram)]
        (println (step-title-fn step-name))
        (printf "\tsamples   %s\n" (:samples h))
        (printf "\tstddev    %.2f ms\n" (:stddev h))
        (doseq [k [:min :10% :30% :50% :70% :80% :90% :95% :99% :99.5% :99.9% :max]]
          (printf "\t%5s % 9.2f ms\n" (name k) (get h k)))
        (println "")))))

(defn- print-summary []
  (println "SUMMARY")
  (println "=======")
  (println "All Error Counts:" (pr-str (error-counts @errors)))
  (println "Non-timeout Errors:" (pr-str (non-timeout-errors @errors)))
  (println (rate/rate-report-str @errors)))

(defn- handle-valid-response-msg
  [thread-name {:keys [transit-start transit-end] :as msg} histogram]
  ;; Latency Histogram
  (let [elapsed-microseconds (/ (- transit-end transit-start) 1000)]
    ;; (println "msg took " elapsed "µs")
    (rate/record-step-transit-time transit-start transit-end)
    (h/record histogram elapsed-microseconds)))

(defn- handle-error-response-msg [thread-name msg]
  (swap! errors conj (assoc msg :thread-name thread-name)))

(defn- handle-response-msg
  [step-title-fn thread-name msg response-num step-name->histogram]
  (if (:error msg)
    (handle-error-response-msg thread-name msg)
    (handle-valid-response-msg thread-name msg (step-name->histogram (:step-name msg))))
  (print-histograms step-title-fn step-name->histogram response-num))

(defn- maybe-clear-histograms [step-name->histogram]
  (when @clear-histograms?
    (println "Resetting histograms")
    (reset! clear-histograms? false)
    (doseq [histogram (vals step-name->histogram)]
      (h/reset histogram))))

(defn- add-histogram [existing {:keys [step-name]}]
  (if (and step-name
           (contains? existing step-name))
    existing
    (assoc existing step-name (h/create histogram-ceiling 2))))

(defn- ->response-worker [step-title-fn]
  (fn [thread-name args]
    (println "Starting response worker thread" thread-name)
    (loop [i 0
           histograms {}]
      (maybe-clear-histograms histograms)
      (if-let [msg (.poll response-q 500 TimeUnit/MILLISECONDS)]
        (let [next-histograms (add-histogram histograms msg)]
          (try
            (handle-response-msg step-title-fn thread-name msg i next-histograms)
            (catch Exception e
              (prn e)
              (.printStackTrace e)))
          (if @stop-workers?
            (println "Stopping response worker thread" thread-name)
            (recur (inc i) next-histograms)))
        (recur (inc i) histograms)))))

(defn- session-completed [_state]
  ;; TODO: We can log the end state of every session if we want.
  (swap! num-msgs-processed inc))

(defn- session-worker-loop [scheduler thread-name args]
  (println (str "Starting session worker thread " thread-name))
  (loop []
    (when-let [session (.poll session-q 500 TimeUnit/MILLISECONDS)]
      (when-let [result-session (scheduler session #(.offer response-q %) session-completed)]
        (.offer session-q result-session)))
    (if @stop-workers?
      (println "Stopping request worker thread")
      (recur))))

(defn- start-response-worker [step-title-fn]
  (reset! stop-workers? false)
  (println "Starting resp worker")
  (thread/start-thread-pool 1 "resp-worker-" (->response-worker step-title-fn))
  (println "Started resp worker"))

(defn- start-session-workers [n scheduler]
  (reset! stop-workers? false)
  (println "Starting session worker")
  (thread/start-thread-pool n "session-worker-" (partial session-worker-loop scheduler))
  (println "Started session worker"))

(defn reset-state! []
  (reset! errors [])
  (reset! clear-histograms? true)
  (reset! num-msgs-processed 0)
  (rate/reset-state!))

(defn- wait-until-all-sessions-processed [num-sessions]
  (while (not= num-sessions @num-msgs-processed)
    (Thread/sleep 200)))

(defn start-workers [step-title-fn num-workers scheduler]
  (start-session-workers num-workers scheduler)
  (start-response-worker step-title-fn))

(defn stop-workers! []
  (reset! stop-workers? true))

(defn run-and-report [sessions]
  (reset-state!)
  (doseq [session sessions]
    (.offer session-q session))
  (wait-until-all-sessions-processed (count sessions))
  (print-summary))

(comment
  (require '[kits.load-driver.sessions :as sessions])

  (def test-session (sessions/->session [(fn [state]
                                           (let [start (System/nanoTime)]
                                             (Thread/sleep 100)
                                             {:status nil
                                              :step-name :test-step
                                              :transit-start start
                                              :transit-end (System/nanoTime)
                                              :error nil
                                              :exception nil
                                              :state state}))]))

  (start-workers (fn [step-name]
                   (str "\n====== STEP: " step-name))
                 5
                 (sessions/->round-robin {:ms-between-session 3}))
  (run-and-report (repeat 5 test-session))
  (reset-state!)
  (stop-workers!))
