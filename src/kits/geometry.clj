(ns kits.geometry)

(def ^:const earth-radius-km 
  "Earth radius (volumetric mean - NASA)" 
  6371 )

(def ^:const max-error-ratio 1e-4)

(defn square-dbl [^:double dbl-val]
  (* dbl-val dbl-val))

(defn haversine 
  "Returns the haversine function of an angle in radians."
  [^:double theta]
  (square-dbl (Math/sin (/ theta 2))) )

(defn haversine-great-circle-angle
  "Returns the angle in radians between to lat/lon points in radians."
  [^:double lat1 ^:double lon1  
   ^:double lat2 ^:double lon2 ]
  (* 2 (Math/asin 
         (Math/sqrt
           (+ (haversine (- lat1 lat2))
              (* (Math/cos lat1) (Math/cos lat2) 
                 (haversine (- lon1 lon2)) ))))))

(defn haversine-great-circle-km
  "Returns the distance in kilometers between to lat/lon points in degrees"
  [^:double lat1-deg ^:double lon1-deg  
   ^:double lat2-deg ^:double lon2-deg]
  (let [lat1-rad (Math/toRadians lat1-deg)
        lon1-rad (Math/toRadians lon1-deg)
        lat2-rad (Math/toRadians lat2-deg)
        lon2-rad (Math/toRadians lon2-deg)

        gc-angle-rad (haversine-great-circle-angle  lat1-rad lon1-rad 
                                                    lat2-rad lon2-rad )
        gc-dist-km (* gc-angle-rad earth-radius-km) ]
      gc-dist-km ))

(defn -main []
  (println)
  (println "*** in main ***")
  (println)
)
