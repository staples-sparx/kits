(ns kits.benchmark
  "A tool for timing code"
  (:require [org.rathore.amit.utils.logger :as log]))

(def ^:dynamic timings nil)

(defn benchmark* [message body-fn]
  (binding [timings (atom [])]
    (let [start (.getTime (java.util.Date.))]
      (try
        (body-fn)
        (finally
         (let [end (.getTime (java.util.Date.))]
           (log/log-message (str "\n"
                                 message
                                 (apply str #(map (str "\n    - " (- (:end %) (:start %)) "ms " (:msg %))
                                                  @timings))
                                 "\n    - Total " (- end start) "ms "
                                 "\n"))))))))

(defmacro benchmark [message & body]
  `(benchmark* ~message (fn [] ~@body)))

(defn timing* [message body-fn]
  (if timings  ; Do not necessarily have to wrap code with benchmark in a repl
     (let [start (.getTime (java.util.Date.))]
       (try
         (body-fn)
         (finally
           (swap! timings conj {:start start :end (.getTime (java.util.Date.)) :msg ~message}))))
     (body-fn)))

(defmacro timing [message & body]
  `(timing* ~message (fn [] ~@body)))

