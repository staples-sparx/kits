(ns kits.random
  "Performant random number generation in a multi-threaded environment.

   java.util.Random severely hurts performance in a multi-threaded
   context due to a high-degree of locking and contention.

   This library uses java.util.concurrent.ThreadLocalRandom instead,
   which exhibits a much less overhead and contention."
  (:import
    java.util.concurrent.ThreadLocalRandom))

(defn thread-local-random []
    (ThreadLocalRandom/current))

(defn ^double next-double [^double n]
  (.nextDouble
    (ThreadLocalRandom/current)
    n))

(defn ^long next-int [^long start ^long end]
  (.nextInt
    (ThreadLocalRandom/current)
    start end))

(defn ^long next-long [n]
  (.nextLong
    (ThreadLocalRandom/current)
    n))
