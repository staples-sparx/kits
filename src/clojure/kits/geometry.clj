(ns kits.geometry
    "Functions for geometric calculations." )

(set! *warn-on-reflection* true)

(def ^:const earth-radius-km
  "Earth radius (volumetric mean - NASA)"
  6371 )

(def ^:const deg->km
  "Distance in km for each degree on earth's surface (spherical model)"
  (-> earth-radius-km
      (* 2 Math/PI)
      (/ 360)
      (double) ))

(defn square-dbl
  "Returns the square of a value as a double"
  [val]
  (let [dbl-val (double val)]
    (* dbl-val dbl-val) ))

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

(defn great-circle-dist-km
  "Returns the distance in kilometers between to lat/lon points in degrees
  (haversine algorithm)."
  [lat1-deg lon1-deg
   lat2-deg lon2-deg]
  (let [lat1-rad (Math/toRadians (double lat1-deg))
        lon1-rad (Math/toRadians (double lon1-deg))
        lat2-rad (Math/toRadians (double lat2-deg))
        lon2-rad (Math/toRadians (double lon2-deg))

        gc-angle-rad (great-circle-radians-haversine  lat1-rad lon1-rad
                                                      lat2-rad lon2-rad )
        gc-dist-km (* gc-angle-rad earth-radius-km) ]
      gc-dist-km ))

(defn simple-dist-km
  "Returns the approximate (flat-earth) distance in kilometers between to lat/lon points
  in degrees."
  [lat1-deg lon1-deg
   lat2-deg lon2-deg]
  (let [mean-lat         (Math/toRadians (/ (double (+ lat1-deg lat2-deg)) 2))
        lat-cos          (Math/cos mean-lat)
        lat-dist-deg     (double            (- lat1-deg lat2-deg))
        lon-dist-deg     (double (* lat-cos (- lon1-deg lon2-deg)))
        dist-deg         (Math/sqrt (+ (square-dbl lat-dist-deg)
                                       (square-dbl lon-dist-deg) ))
        dist-km          (* dist-deg deg->km) ]
    dist-km ))
