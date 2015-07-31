(ns kits.load-driver.histograms
  (:import (org.HdrHistogram Histogram)))

(defn create [highest-trackable-value number-of-significant-value-digits]
  (Histogram. highest-trackable-value number-of-significant-value-digits))

(defn reset [^Histogram h]
  (.reset h))

(defn record [^Histogram h value]
  (.recordValue h value))

(defn num-samples [^Histogram h]
  (-> h .getHistogramData .getTotalCount long))

(defn- ->millis [n]
  (double (/ n 1000)))

(defn collect [^Histogram h]
  (let [data (.getHistogramData h)]
    {:samples (long (.getTotalCount data))
     :stddev (->millis (.getStdDeviation data))
     :min    (->millis (.getMinValue data))
     :mean   (->millis (.getMean data))
     :max    (->millis (.getMaxValue data))
     :10%    (->millis (.getValueAtPercentile data 10))
     :30%    (->millis (.getValueAtPercentile data 30))
     :50%    (->millis (.getValueAtPercentile data 50))
     :70%    (->millis (.getValueAtPercentile data 70))
     :80%    (->millis (.getValueAtPercentile data 80))
     :90%    (->millis (.getValueAtPercentile data 90))
     :95%    (->millis (.getValueAtPercentile data 95))
     :99%    (->millis (.getValueAtPercentile data 99))
     :99.5%  (->millis (.getValueAtPercentile data 99.5))
     :99.9%  (->millis (.getValueAtPercentile data 99.9))
     ;; :over_1ms (.getCountBetweenValues data 1000 (.getHighestTrackableValue h))
     ;; :over_5ms (.getCountBetweenValues data 5000 (.getHighestTrackableValue h))
     ;; :over_10ms (.getCountBetweenValues data 10000
     ;; (.getHighestTrackableValue h))
     }))
