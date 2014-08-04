(ns ^{:doc "Statistical calculation functions"}
  kits.statistics
  (:import (umontreal.iro.lecuyer.probdist StudentDist)))

(defn- variance [summed-squares sum n]
  "Calculate sample variance given the sum of the squares of each datapoint,
the sum, and the count."
  (/ (- summed-squares
        (/ (Math/pow sum 2)
           n))
     (dec n)))

(defn- get-t-statistic [modelled-mean modelled-variance modelled-count
                       control-mean control-variance control-count]
  (/ (- modelled-mean control-mean)
     (Math/sqrt
      (+ (/ modelled-variance modelled-count)
         (/ control-variance control-count)))))

(defn- get-degrees-of-freedom [test-variance test-count
                              control-variance control-count]
  (let [test-v (/ test-variance test-count)
        control-v (/ control-variance control-count)]
    (/ (Math/pow (+ test-v control-v) 2)
       (+ (/ (Math/pow test-v 2) (dec test-count))
          (/ (Math/pow control-v 2) (dec control-count))))))

(defn- create-student-distribution [degrees-of-freedom t-statistic]
  (.cdf (StudentDist. degrees-of-freedom) t-statistic))

(defn calculate-confidence-level
  [num-modelled-sessions total-modelled-var total-squared-modelled-var
   num-control-sessions total-control-var total-squared-control-var]
  (try
    (let [modelled-mean      (/ total-modelled-var num-modelled-sessions)
          control-mean       (/ total-control-var num-control-sessions)
          modelled-variance  (variance total-squared-modelled-var
                                       total-modelled-var
                                       num-modelled-sessions)
          control-variance   (variance total-squared-control-var
                                       total-control-var
                                       num-control-sessions)
          t-statistic        (get-t-statistic modelled-mean
                                              modelled-variance
                                              num-modelled-sessions
                                              control-mean
                                              control-variance
                                              num-control-sessions)
          degrees-of-freedom (get-degrees-of-freedom modelled-variance
                                                     num-modelled-sessions
                                                     control-variance
                                                     num-control-sessions)]
      (if (or (Double/isNaN degrees-of-freedom) (Double/isNaN t-statistic))
        -1 ;; not enough data
        (create-student-distribution degrees-of-freedom t-statistic)))
    (catch ArithmeticException e
      -1)))

(defn calculate-binary-confidence-level
  [num-modelled-sessions total-modelled-var num-control-sessions total-control-var]
  ;; NOTE: the sum of the squares of binary datapoints = sum of datapoints
  (calculate-confidence-level num-modelled-sessions
                              total-modelled-var
                              total-modelled-var
                              num-control-sessions
                              total-control-var
                              total-control-var))

