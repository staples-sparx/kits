(ns kits.geometry)

(def ^:const earth-radius-km 
  "Earth radius (volumetric mean - NASA)" 
  6371 )

(def ^:const max-error-ratio 1e-4)

(defn haversine-distance-km [from-lat from-lon to-lat to-lon]
  (let [sin-dlat (Math/sin (/ (Math/toRadians (- to-lat from-lat)) 2))
        sin-dlon (Math/sin (/ (Math/toRadians (- to-lon from-lon)) 2))
        a (+ (Math/pow sin-dlat 2)
             (* (Math/pow sin-dlon 2)
                (Math/cos (Math/toRadians from-lat))
                (Math/cos (Math/toRadians to-lat))))]
    (* earth-radius-km 
       (* 2 (Math/atan2 (Math/sqrt a) (Math/sqrt (- 1 a)))
    ))))

(defn -main []
  (let [dist-kits (haversine-distance-km  33.088678 -117.242227
                                          37.561820 -122.324625)
        dist-ref  677.9969
        delta     (Math/abs (- dist-ref dist-kits)) 
        ratio     (/ delta earth-radius-km)
  ]
    (assert (< ratio max-error-ratio))
  )
  (let [dist-kits (haversine-distance-km  33.088678 -117.242227
                                          40.689215, -74.044627)
        dist-ref  3892.9607
        delta     (Math/abs (- dist-ref dist-kits)) 
        ratio     (/ delta earth-radius-km)
  ]
    (assert (< ratio max-error-ratio))
  )

  (println)
  (println "*** all tests passed ***")
  (println)
)
