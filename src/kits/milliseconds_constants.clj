(ns kits.milliseconds-constants
  "Time unit constants"
  (:refer-clojure :exclude [second]))

(def ^:const second 1000)
(def ^:const minute (* 60 second))
(def ^:const hour (* 60 minute))
(def ^:const day (* 24 hour))
(def ^:const week (* 7 day))
(def ^:const year (* 365 day))
