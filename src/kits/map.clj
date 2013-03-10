(ns kits.map
  "Functions that operate on Clojure maps."
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [clojure.walk :as w]
            [kits.string :as kstr]))


;;; Mapping and Filtering Over Maps

(defn filter-map
  "given a predicate like (fn [k v] ...) returns a map with only entries that match it."
  [pred m]
  (into {}
    (filter (fn [[k v]] (pred k v))
      m)))

(defn filter-by-key
  "given a predicate like (fn [k] ...) returns a map with only entries with keys that match it."
  [pred m]
  (into {}
    (filter (fn [[k v]] (pred k))
      m)))

(defn filter-by-val
  "given a predicate like (fn [v] ...) returns a map with only entries with vals that match it."
  [pred m]
  (into {}
    (filter (fn [[k v]] (pred v))
      m)))

(defn map-over-map
  "given a function like (fn [k v] ...) returns a new map with each entry mapped by it."
  [f m]
  (into {}
    (map (fn [[k v]] (f k v))
      m)))

(defn map-keys
  "Apply a function on all keys of a map and return the corresponding map (all values untouched)"
  [f m]
  (zipmap
    (map f (keys m))
    (vals m)))

(defn map-values
  "Apply a function on all values of a map and return the corresponding map (all keys untouched)"
  [f m]
  (zipmap
    (keys m)
    (map f (vals m))))


;;; Everything Else

(defn assoc-if-not-present
  "Like assoc, except only adds the key-value pair if the key doesn't exist in the map"
  ([m k v]
     (if (contains? m k)
       m
       (assoc m k v)))
  ([m k v & kvs]
     (let [ret (if (contains? m k)
                 m
                 (assoc m k v))]
       (if kvs
         (recur ret (first kvs) (second kvs) (nnext kvs))
         ret))))

(defn contains-path? [m path]
  (and (not (empty? path))
       (not= ::not-found (get-in m path ::not-found))))

(defn assoc-in-if-present [m path f]
  (if (contains-path? m path)
    (assoc-in m path f)
    m))

(defn dissoc-in
  "Dissociates an entry from a nested associative structure returning a new
  nested structure. path is a sequence of keys. Any empty maps that result
  will not be present in the new structure."
  ([m [k & ks :as path]]
    (if ks
      (if-let [nextmap (get m k)]
        (let [newmap (dissoc-in nextmap ks)]
          (if (seq newmap)
            (assoc m k newmap)
            (dissoc m k)))
        m)
      (dissoc m k)))
  ([m path & more-paths]
    (apply dissoc-in (dissoc-in m path) more-paths)))

;; Alex - TODO - 6/23/12 (M/D/Y) - copied from contrib.
;; Add some unit tests

(defn deep-merge-with
  "Like merge-with, but merges maps recursively, applying the given fn
  only when there's a non-map at a particular level.

  (deep-merge-with + {:a {:b {:c 1 :d {:x 1 :y 2}} :e 3} :f 4}
                     {:a {:b {:c 2 :d {:z 9} :z 3} :e 100}})
  -> {:a {:b {:z 3, :c 3, :d {:z 9, :x 1, :y 2}}, :e 103}, :f 4}"
  [f & maps]
  (apply
   (fn m [& maps]
     (if (every? map? maps)
       (apply merge-with m maps)
       (apply f maps)))
   maps))

(defn invert-map [m]
  (zipmap (vals m) (keys m)))

(defn keys-to-keywords [m & {:keys [underscore-to-hyphens?]
                             :or {underscore-to-hyphens? true}}]
  (if-not (map? m)
    m
    (zipmap
     (if underscore-to-hyphens?
       (map #(-> % (str/replace "_" "-") keyword) (keys m))
       (map keyword (keys m)))
     (map #(keys-to-keywords % :underscore-to-hyphens? underscore-to-hyphens?) (vals m)))))

(defn map-difference [m1 m2]
  (let [ks1 (set (keys m1))
        ks2 (set (keys m2))
        ks1-ks2 (set/difference ks1 ks2)
        ks2-ks1 (set/difference ks2 ks1)
        ks1*ks2 (set/intersection ks1 ks2)]
    (merge (select-keys m1 ks1-ks2)
           (select-keys m2 ks2-ks1)
           (select-keys m1
                        (remove (fn [k] (= (m1 k) (m2 k)))
                                ks1*ks2)))))

(defn nested-dissoc
  "dissoc keys from every map at every level of a nested data structure"
  [data & ks]
  (cond (map? data)
    (let [map-minus-ks (apply dissoc data ks)]
      (map-values #(apply nested-dissoc % ks) map-minus-ks))

    (set? data)
    (into (empty data) (map #(apply nested-dissoc % ks) data))

    (sequential? data)
    (map #(apply nested-dissoc % ks) data)

    :else
    data))

(defn paths
  "Return the paths of the leaves in the map"
  [m]
  (when m
    (letfn [(key-paths [prefix m]
              (if (map? m)
                (into {} (map (fn [[k v]] (key-paths (conj prefix k) v)) m))
                {prefix m}))]
      (keys (key-paths [] m)))))

(defn rmerge
  "Recursive merge of the provided maps."
  [& maps]
  (if (every? map? maps)
    (apply merge-with rmerge maps)
    (last maps)))

(defn select-paths [m & paths]
  (reduce
    (fn [result path]
      (assoc-in result path (get-in m path)))
    {}
    paths))

(defn subpath?
  "true if 'path' is a child of 'root-path'"
  [root-path path]
  (= root-path
    (take (count root-path) path)))

(defn subpaths [path]
  (rest (reductions conj [] path)))

(defn transform-keywords [f m]
  (w/postwalk #(if (keyword? %)
               (f %)
               %)
    m))

(defn keywords->hyphenated-keywords [m]
  (transform-keywords kstr/keyword->hyphenated-keyword m))

(defn keywords->underscored-keywords [m]
  (transform-keywords kstr/keyword->underscored-keyword m))

(defn keywords->underscored-strings [m]
  (transform-keywords kstr/keyword->underscored-string m))

(defn update-in-if-present [m path f]
  (if (contains-path? m path)
    (update-in m path f)
    m))

(defn submap? [sub-map m]
  (every? (fn [[k v]]
            (= v (get m k)))
          sub-map))

(defn select-keys-always
  ([m ks]
     (select-keys-always m ks nil))
  ([m ks default]
     (into {}
           (for [k ks]
             [k (get m k default)]))))

(defn move-key [m old-key new-key]
  (let [v (get m old-key ::missing)]
    (if (= v ::missing)
      m
      (-> m
          (assoc new-key v)
          (dissoc old-key)))))