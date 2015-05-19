(ns kits.random
  (:import (java.util.concurrent ThreadLocalRandom)))

(defn- thread-local-random []
  (ThreadLocalRandom/current))

(defn next [bits]
  (.next (thread-local-random) bits))

(defn next-double [n]
  (.nextDouble (thread-local-random) n))

(defn next-int [start end]
  (.nextInt (thread-local-random) start end))

(defn next-long [n]
  (.nextLong (thread-local-random) n))

