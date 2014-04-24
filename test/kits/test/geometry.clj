(ns kits.test.geometry
  (:use clojure.test)
  (:require [kits.geometry :as geom] ))

(def ^:private ^:const pi              Math/PI   )
(def ^:private ^:const pi-ovr-2     (/ Math/PI 2))
(def ^:private ^:const pi-ovr-4     (/ Math/PI 4))

(def ^:private ^:const max-error-ratio 1e-4)

(defn error-ratio
  [ ^:double ref-val ^:double val1 ^:double val2 ]
  (/ (Math/abs (- val1 val2))
     ref-val))

(deftest great-circle-angle
  (testing "Basic great circle angle calc"
    (is (= 4 (* 2 2)))
    (is (< 1e-5 max-error-ratio))

    ; equator to north pole
    (is (> max-error-ratio
           (error-ratio 1 pi-ovr-2  
                        (geom/haversine-great-circle-angle 0 0 pi-ovr-2 0) )))
    ; 90 deg spanning equator
    (is (> max-error-ratio
           (error-ratio 1 pi-ovr-2
                        (geom/haversine-great-circle-angle (- pi-ovr-4) 2 pi-ovr-4 2) )))

    ; 90 deg on equator
    (is (> max-error-ratio
           (error-ratio 1 pi-ovr-2
                        (geom/haversine-great-circle-angle 0 (- pi-ovr-4) 0 pi-ovr-4) )))

  )
)

