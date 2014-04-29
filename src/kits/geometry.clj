(ns kits.geometry
  "Functions for geometric calculations." )

(set! *warn-on-reflection* true)

(def ^:const earth-radius-km 
  "Earth radius (volumetric mean - NASA)" 
  6371 )

(defn square-dbl 
  "Square a double value." 
  [^:double dbl-val]
  (* dbl-val dbl-val))

(defn haversine 
  "Returns the haversine function of an angle in radians."
  [^:double theta]
  (square-dbl (Math/sin (/ theta 2))) )

(defn great-circle-radians-haversine
  "Returns the angle in radians between to lat/lon points in radians."
  [^:double lat1 ^:double lon1  
   ^:double lat2 ^:double lon2]
  (* 2 (Math/asin 
         (Math/sqrt
           (+ (haversine (- lat1 lat2))
              (* (Math/cos lat1) (Math/cos lat2) 
                 (haversine (- lon1 lon2)) ))))))

(defn great-circle-km
  "Returns the distance in kilometers between to lat/lon points in degrees (haversine algorithm)."
  [^:double lat1-deg ^:double lon1-deg  
   ^:double lat2-deg ^:double lon2-deg]
  (let [lat1-rad (Math/toRadians lat1-deg)
        lon1-rad (Math/toRadians lon1-deg)
        lat2-rad (Math/toRadians lat2-deg)
        lon2-rad (Math/toRadians lon2-deg)

        gc-angle-rad (great-circle-radians-haversine  lat1-rad lon1-rad 
                                                      lat2-rad lon2-rad )
        gc-dist-km (* gc-angle-rad earth-radius-km) ]
      gc-dist-km ))

