(ns kits.seq
  (:require [clojure.walk :as walk]))


(defn any? [pred coll]
  (boolean (some pred coll)))

(defn butlastv
  "Like (vec (butlast v))' but efficient for vectors"
  [v]
  (let [cnt (count v)]
    (if (< cnt 2)
      []
      (subvec v 0 (dec cnt)))))

(defn ensure-sequential [x]
  (if (sequential? x)
    x
    [x]))

(defn indexed
  "Returns a lazy sequence of [index, item] pairs, where items come
  from 's' and indexes count up from zero.

  (indexed '(a b c d))  =>  ([0 a] [1 b] [2 c] [3 d])"
  [s]
  (map vector (iterate inc 0) s))

(defn max-by [sort-by-fn xs]
  (last (sort-by sort-by-fn xs)))

(defn min-by [sort-by-fn xs]
  (first (sort-by sort-by-fn xs)))

(defn only
  "Gives the sole element of a sequence"
  [coll]
  (if (seq (rest coll))
    (throw (RuntimeException. "should have precisely one item, but had at least 2"))
    (if (seq coll)
      (first coll)
      (throw (RuntimeException. "should have precisely one item, but had 0")))))

(defn rand-take [coll n]
  (cond (nil? coll)
        []

        (> n (count coll))
        (shuffle coll)

        :else
        (take n (shuffle coll))))

(defn realize [x]
  (walk/postwalk identity x))

(defn segregate
  "Splits the collection into two collections of the same type. The first
   collection contains all elements that pass the predicate and the second
   collection all the items that fail the predicate."
  [pred coll]
  (reduce (fn [[passes fails] elem]
            (if (pred elem)
              [(conj passes elem) fails]
              [passes (conj fails elem)]))
    [(empty coll) (empty coll)]
    coll))

(defn seq-to-map
  "Transforms a seq of ([key1 value1] [key2 value2]) pairs to a map
   {key1 value1 key2 value2}. For empty and nil values, returns nil."
  [coll]
  (when (seq coll)
    (into {} coll)))

(defn zip
  "[[:a 1] [:b 2] [:c 3]] ;=> [[:a :b :c] [1 2 3]]"
  [seqs]
  (if (empty? seqs)
    []
    (apply map list seqs)))

