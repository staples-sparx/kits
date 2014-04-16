(ns kits.job
  "A library that models a series of steps, each of which can abort the workflow"
  (:require [kits.logging.log-async :as log]))

(defn aborting? [job]
  (:aborting job))

(defn abort-job [job & error-messages]
  (log/error {:job-id (:job-id job) :message "Aborting Job" :error-messages error-messages})
  (assoc job
    :aborting true
    :job-errors (into (:job-errors job) error-messages)))

(defn abort-job-with-exception
  ([job message ^Exception e]
     (abort-job job message (.getMessage e) (clojure.string/join "\n" (.getStackTrace e))))
  ([job ^Exception e]
     (abort-job job (.getMessage e) (clojure.string/join "\n" (.getStackTrace e)))))

(defn warn-job [job & warning-messages]
  (log/warn {:job-id (:job-id job) :message warning-messages})
  (assoc job
    :job-warnings (into (:job-warnings job) warning-messages)))

(defn warn-job-with-exception
  ([job message ^Exception e]
     (warn-job job message (.getMessage e) (clojure.string/join "\n" (.getStackTrace e))))
  ([job ^Exception e]
     (warn-job job (.getMessage e) (clojure.string/join "\n" (.getStackTrace e)))))

(defn run-with-short-circuiting [job initial-argument functions]
  "Each function accepts [argument, job] and returns [result, job],
   which becomes the argument to the next function"
  (loop [argument initial-argument
         job job
         [first-fn & rest] functions]
    (if (or (not first-fn)
            (aborting? job))
      [argument job]
      (let [[result job] (first-fn argument job)]
        (recur result job rest)))))

(defn create-job
  ([] (create-job nil))
  ([params]
     {:job-id (java.util.UUID/randomUUID)
      :aborting false
      :job-errors []
      :job-warnings []
      :params params}))
