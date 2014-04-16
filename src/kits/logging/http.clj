(ns kits.logging.http
  "Logging macro for HTTP handlers to measure performance benchmarks."
  (:require [kits.queues :as q]
            [kits.logging.log-async :as log-async]))

(defn- nano-time
  "Returns the current value of the most precise available system timer, in nanoseconds."
  []
  (System/nanoTime))

(defmacro _div [a b] `(/ (long ~a) (long ~b)))

(defn- nano->micro [ts]
  (_div ts 1000))

(defn- micro->milli [ts]
  (_div ts 1000))

(defn- micro-time
  "Returns number of micro-seconds since the epoch"
  []
  (long (nano->micro (nano-time))))

(defn- ms-time
  " Returns number of milli-seconds since the epoch"
  []
  (System/currentTimeMillis))

(defn value-with-times
  "Return the value of `thunk` along with start and ending times in
  milliseconds and elapsed-time in microseconds, e.g.,

  (value-with-times (fn [] :foo))
  => [:foo start-time-ms end-time-ms elapsed-time-us]."
  [thunk]
  (let [start (ms-time)
        start-us (micro-time)
        value (thunk)
        end-us (micro-time)]
    [value start (ms-time) (- end-us start-us)]))

(defn log-api-call [log-q request params reply start end elapsed]
  (q/add log-q
         {:request request
          :params params
          :reply reply
          :meta {:start start :end end :elapsed elapsed}}))

(defmacro bind-with-times [bindings expr & body]
  `(let [~bindings (value-with-times (fn [] ~expr))]
     ~@body))

(defmacro wrap-with-logging [log-q request params & body]
  "Wrap with logging into some queue. Pass in nil to use the default queue"
  `(bind-with-times [reply# start# end# elapsed#]
     (do ~@body)
     (log-api-call (or ~log-q @log-async/log-q) ~request ~params reply# start# end# elapsed#)
     reply#))
