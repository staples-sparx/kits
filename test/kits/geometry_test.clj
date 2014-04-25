(ns kits.geometry-test
  (:require  [kits.geometry :as geom] )
  (:use      [clojure.test] )
)

(set! *warn-on-reflection* false)

(def ^:private ^:const pi              Math/PI   )   ; 180 deb
(def ^:private ^:const pi-ovr-2     (/ Math/PI 2))   ;  90 deg
(def ^:private ^:const pi-ovr-4     (/ Math/PI 4))   ;  45 deg

(def ^:private ^:const max-error-ratio 1e-5)

(defn error-ratio
  [ ^:double val1 ^:double val2 ]
  (let [abs-delta    (Math/abs (- val1 val2))
        max-abs-val  (Math/max (Math/abs val1)
                               (Math/abs val2) )
        error-ratio  (/ abs-delta max-abs-val) ]
    error-ratio ))

(deftest great-circle-radians
  (testing "Great circle angle calc"
    (is (= 4 (* 2 2)))

    ; equator to north pole
    (is (> max-error-ratio
            (error-ratio  pi-ovr-2 (geom/great-circle-radians-haversine 0 0 pi-ovr-2 0)) ))

    ; 90 deg spanning equator
    (is (> max-error-ratio
            (error-ratio  pi-ovr-2 (geom/great-circle-radians-haversine (- pi-ovr-4) 2 pi-ovr-4 2)) ))

    ; 90 deg spanning equator
    (is (> max-error-ratio
            (error-ratio  pi-ovr-2 (geom/great-circle-radians-haversine (- pi-ovr-4) 2 pi-ovr-4 2)) ))

    ; 90 deg on equator
    (is (> max-error-ratio
            (error-ratio  pi-ovr-2 (geom/great-circle-radians-haversine 0 (- pi-ovr-4) 0 pi-ovr-4)) ))
  )
  (testing "Great circle dist calc"
    ; Carlsbad to Staples Labs
    (is (let [dist-kits (geom/great-circle-km  33.088678 -117.242227
                                               37.561820 -122.324625)
              dist-ref  677.9969 ]
          (> max-error-ratio (error-ratio  dist-ref dist-kits)) ))
    ; Carlsbad to Statue of Liberty
    (is (let [dist-kits (geom/great-circle-km  33.088678 -117.242227
                                               40.689215, -74.044627)
              dist-ref  3892.9607 ]
          (> max-error-ratio (error-ratio  dist-ref dist-kits)) ))
  )
)
