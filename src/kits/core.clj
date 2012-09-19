(ns kits.core
  (:use clojure.pprint
        clojure.walk)
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.java.shell :as sh]
            [kits.foundation :as f])
  (:import [java.io File FileOutputStream]))


;;; Maps Utils -- TODO Alex Sep 18, 2012 - pull out to `kits.maps` ns

(defn transform-keywords [f m]
  (postwalk #(if (keyword? %)
               (f %)
               %)
    m))

(defn keyword->hyphenated-string [k]
  (-> k name (.replace "_" "-")))

(defn keyword->hyphenated-keyword [k]
  (-> k keyword->hyphenated-string keyword))

(defn keyword->underscored-string [k]
  (-> k name (.replace "-" "_")))

(defn keyword->underscored-keyword [k]
  (-> k keyword->underscored-string keyword))

(defn keywords->hyphenated-keywords [m]
  (transform-keywords keyword->hyphenated-keyword m))

(defn keywords->underscored-strings [m]
  (transform-keywords keyword->underscored-string m))

(defn keywords->underscored-keywords [m]
  (transform-keywords keyword->underscored-keyword m))

(defn invert-map [m]
  (zipmap (vals m) (keys m)))

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

(defn paths
  "Return the paths of the leaves in the map"
  [m]
  (when m
    (letfn [(key-paths [prefix m]
              (if (map? m)
                (into {} (map (fn [[k v]] (key-paths (conj prefix k) v)) m))
                {prefix m}))]
      (keys (key-paths [] m)))))

(defn subpaths [path]
  (rest (reductions conj [] path)))

(defn subpath?
  "true if 'path' is a child of 'root-path'"
  [root-path path]
  (= root-path
    (take (count root-path) path)))

(defn select-paths [m & paths]
  (reduce
    (fn [result path]
      (assoc-in result path (get-in m path)))
    {}
    paths))

(defn contains-path? [m path]
  (and (not (empty? path))
    (not= ::not-found (get-in m path ::not-found))))

(defn update-in-if-present [m path f]
  (if (contains-path? m path)
    (update-in m path f)
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

;; Alex - TODO - 6/23/12 (M/D/Y) - copied from contrib.
;;    Add some unit tests

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

(defn keys-to-keywords [m & {:keys [underscore-to-hyphens?]
                             :or {underscore-to-hyphens? true}}]
  (if-not (map? m)
    m
    (zipmap
      (if underscore-to-hyphens?
        (map #(-> % (str/replace "_" "-") keyword) (keys m))
        (map keyword (keys m)))
      (map #(keys-to-keywords % :underscore-to-hyphens? underscore-to-hyphens?) (vals m)))))

;; END Map Utils

(defn raise [& x]
  (throw (Exception. ^String (apply str x))))

(defn print-vals [& args]
  (apply println (cons "*** " (map #(if (string? %) % (with-out-str (pprint %)))  args)))
  (last args))

(defn parse-number
  ([s]
    (parse-number s nil))
  ([s default-value]
    (cond
      (number? s)  s
      (empty? s)  default-value
      :default (read-string s))))


(defn read-string-safely [s] (when s (read-string s)))


(defn trap-nil [x default]
  (if-not (nil? x) x default))

(defn rand-int* [min max]
  (+ min (rand-int (- max min))))

(defn print-error
  "Println to *err*"
  [& args]
  (binding [*out* *err*]
    (apply println args)))

(defn return-zero-if-negative [number]
  (try
    (if (pos? number)
      number
      0)
    (catch Exception e
      "Not Available")))

(defn sget
  "Safe get. Get the value of key `k` from map `m` only if the key really
  exists, throw exception otherwise."
  [m k] {:pre [(map? m)]}
  (assert (contains? m k))
  (get m k))

(defmacro time-elapsed
  "Returns time elapsed in millis."
  [& body]
  `(let [start# (. System (nanoTime))]
     ~@body
     (/ (double (- (. System (nanoTime)) start#)) 1000000.0)))

(defn timeout-fn
  "Take one function and wrap it such that it times out if it takes too long. Say you are
   querying HBase and don't want to wait for a map/filter that may take 10 min or more"
  [millis f]
  (fn [& args]
    (let [f-with-args-curried (fn [] (apply f args))
          fut (future-call f-with-args-curried)]
      (try
        (.get ^java.util.concurrent.Future fut
          millis
          java.util.concurrent.TimeUnit/MILLISECONDS)
        (finally
          (future-cancel fut))))))

(defmacro with-timeout
  "Usage: (with-timeout 1000 (execute-long-running-code) (more-code))"
  [millis & body]
  `((timeout-fn ~millis (bound-fn [] ~@body))))

(defn =>
  "true functional thrush"
  [data & fns]
  (reduce #(%2 %1) data fns))

(defn segregate
  "returns [(filter f s) (remove f s)], but only runs through the seq once"
  [f s]
  (reduce (fn [[fl rl] i]
            (if (f i)
              [(conj fl i) rl]
              [fl (conj rl i)]))
    [[] []]
    s))

(defmacro periodic-fn
  "creates a fn that executes 'body' every 'period' calls"
  [args [var period] & body]
  `(let [call-count# (atom 0)]
     (fn [~@args]
       (swap! call-count# inc)
       (when (zero? (mod @call-count# ~period))
         (let [~var @call-count#]
           ~@body)))))

(defn safe-sleep
  "Sleep for `millis` milliseconds."
  [millis]
  (try (Thread/sleep millis)
    (catch InterruptedException e
      (.interrupt ^Thread (Thread/currentThread)))))

(defn random-sleep
  "Sleep between 'min-millis' and 'max-millis' milliseconds"
  [min-millis max-millis]
  (let [range (- max-millis min-millis)
        millis (+ min-millis (rand-int range))]
    (safe-sleep millis)))

(defn wait-until [done-fn? & {:keys [ms-per-loop timeout]
                              :or {ms-per-loop 1000 timeout 10000}}]
  (loop [elapsed (long 0)]
    (when-not (or (>= elapsed timeout) (done-fn?))
      (Thread/sleep ms-per-loop)
      (recur (long (+ elapsed ms-per-loop))))))

(defn boolean? [x]
  (or (= true x) (= false x)))

(defn fn->fn-thats-false-if-throws
  "Takes a fn.  Returns a new fn that evaluates
   to false if it throws an Exception."
  [f]
  (fn [& args]
    (try
      (apply f args)
      (catch Throwable e false))))

(defn non-neg-integer? [x]
  (and (integer? x)
    (not (neg? x))))

(defmacro def-many-methods
  "Creates multiple multimethods with different dispatch values, but the same implementation."
  [name dispatch-values & body]
  `(doseq [dispatch-val# ~dispatch-values]
     (defmethod ~name dispatch-val# ~@body)))

(defn url
  "Returns a java.net.URL instance or nil if URL failed to parse"
  [^String s]
  (when s
    (try
      (java.net.URL. s)
      (catch java.net.MalformedURLException e
        nil))))

(defn url? [s]
  (boolean (url s)))

(def valid-ip-address-v4-re
  #"^([01]?\d\d?|2[0-4]\d|25[0-5])\.([01]?\d\d?|2[0-4]\d|25[0-5])\.([01]?\d\d?|2[0-4]\d|25[0-5])\.([01]?\d\d?|2[0-4]\d|25[0-5])$")

(defn ip-address-v4? [s]
  (boolean (when s
             (re-matches valid-ip-address-v4-re s))))

(defn timestamp? [n]
  (and (integer? n)
    (>= n 0)
    (<= n Long/MAX_VALUE)))

(defn stacktrace->str [e]
  (map #(str % "\n") (.getStackTrace ^Exception e)))

(defn zip
  "[[:a 1] [:b 2] [:c 3]] ;=> [[:a :b :c] [1 2 3]]"
  [seqs]
  (if (empty? seqs)
    []
    (apply map list seqs)))

(defn mkdir-p
  "Create directory and parent directories if any"
  [^String path]
  (let [f ^File (File. path)]
    (.mkdirs f)))

(defn cents->dollar-str [cents]
  (format "%.2f" (/ cents 100.0)))

(defn max-by [sort-by-fn xs]
  (last (sort-by sort-by-fn xs)))

(defn min-by [sort-by-fn xs]
  (first (sort-by sort-by-fn xs)))

(defn nested-sort [x]
  (cond (sequential? x)
    (if (instance? java.lang.Comparable (first x))
      (sort (map nested-sort x))
      (map nested-sort x))

    (map? x)
    (if (and (not= {} x)
          (instance? java.lang.Comparable (key (first x))))
      (into (sorted-map) (map-values nested-sort x))
      (map-values nested-sort x))

    :else
    x))

(def p (comp pprint nested-sort))

(defn non-empty? [x]
  (not (empty? x)))

(defn only
  "Gives the sole element of a sequence"
  [coll]
  (if (seq (rest coll))
    (throw (RuntimeException. "should have precisely one item, but had at least 2"))
    (if (seq coll)
      (first coll)
      (throw (RuntimeException. "should have precisely one item, but had 0")))))

(defn indexed
  "Returns a lazy sequence of [index, item] pairs, where items come
  from 's' and indexes count up from zero.

  (indexed '(a b c d))  =>  ([0 a] [1 b] [2 c] [3 d])"
  [s]
  (map vector (iterate inc 0) s))

(defn name-generator [prefix]
  (let [cnt (atom -1)]
    (fn []
      (swap! cnt inc)
      (str prefix "-" @cnt))))

(defn ensure-sequential [x]
  (if (sequential? x)
    x
    [x]))

(defn retrying-fn
  "Take a no-arg function f, and returns a new no-arg function that
   will call f again if calling f throws a Throwable. f is called a max of 3 times."
  [f]
  (fn this
    ([]
      (this 2))
    ([retry-count]
      (try
        (f)
        (catch Throwable t
          (if (zero? retry-count)
            (throw t)
            (this (dec retry-count))))))))

(defmacro with-retries [& body]
  `((retrying-fn
      (fn [] ~@body))))

(defn transform-fakejson-params->map [m]
  (reduce
    (fn [m [k v]]
      (let [v (when-not (empty? v) v)]
        (assoc-in m (remove empty? (str/split k #"[\[\]]")) v)))
    {}
    m))

(defn any? [pred coll]
  (boolean (some pred coll)))

(defn butlastv
  "Like (vec (butlast v))' but efficient for vectors"
  [v]
  (let [cnt (count v)]
    (if (< cnt 2)
      []
      (subvec v 0 (dec cnt)))))

(defn single-destructuring-arg->form+name
  "Turns any one binding arg (which may be a destructuring binding) into a vector
   where the left elem is the arg with a possible :as added to it.
   And the rght side is the symbol referring to the arg itself."
  [arg-form]
  (let [as-symbol (gensym 'symbol-for-destructured-arg)
        snd-to-last-is-as? #(= :as (second (reverse %)))]
    (cond (and (vector? arg-form) (snd-to-last-is-as? arg-form))
      [arg-form (last arg-form)]

      (vector? arg-form)
      [(-> arg-form (conj :as) (conj as-symbol)) as-symbol]

      (and (map? arg-form) (contains? arg-form :as))
      [arg-form (:as arg-form)]

      (map? arg-form)
      [(assoc arg-form :as as-symbol) as-symbol]

      :else
      [arg-form arg-form])))

(defn clojure-version-as-double
  "Clojure 1.2.1 => 1.21
   Clojure 1.4.0 => 1.4"
  []
  (+ (:major *clojure-version*)
    (/ (:minor *clojure-version*) 10)
    (/ (:incremental *clojure-version*) 100)))

(defmacro when-1-2 [& body]
  (when (= 2 (:minor *clojure-version*))
    `(do ~@body)))

;;;; Copied out of Clojure 1.3+

(when-1-2
  (defn some-fn
    "Takes a set of predicates and returns a function f that returns the first logical true value
    returned by one of its composing predicates against any of its arguments, else it returns
    logical false. Note that f is short-circuiting in that it will stop execution on the first
    argument that triggers a logical true result against the original predicates."
    {:added "1.3"}
    ([p]
      (fn sp1
        ([] nil)
        ([x] (p x))
        ([x y] (or (p x) (p y)))
        ([x y z] (or (p x) (p y) (p z)))
        ([x y z & args] (or (sp1 x y z)
                          (some p args)))))
    ([p1 p2]
      (fn sp2
        ([] nil)
        ([x] (or (p1 x) (p2 x)))
        ([x y] (or (p1 x) (p1 y) (p2 x) (p2 y)))
        ([x y z] (or (p1 x) (p1 y) (p1 z) (p2 x) (p2 y) (p2 z)))
        ([x y z & args] (or (sp2 x y z)
                          (some #(or (p1 %) (p2 %)) args)))))
    ([p1 p2 p3]
      (fn sp3
        ([] nil)
        ([x] (or (p1 x) (p2 x) (p3 x)))
        ([x y] (or (p1 x) (p2 x) (p3 x) (p1 y) (p2 y) (p3 y)))
        ([x y z] (or (p1 x) (p2 x) (p3 x) (p1 y) (p2 y) (p3 y) (p1 z) (p2 z) (p3 z)))
        ([x y z & args] (or (sp3 x y z)
                          (some #(or (p1 %) (p2 %) (p3 %)) args)))))
    ([p1 p2 p3 & ps]
      (let [ps (list* p1 p2 p3 ps)]
        (fn spn
          ([] nil)
          ([x] (some #(% x) ps))
          ([x y] (some #(or (% x) (% y)) ps))
          ([x y z] (some #(or (% x) (% y) (% z)) ps))
          ([x y z & args] (or (spn x y z)
                            (some #(some % args) ps))))))))

(when-1-2
  (defn every-pred
    "Takes a set of predicates and returns a function f that returns true if all of its
    composing predicates return a logical true value against all of its arguments, else it returns
    false. Note that f is short-circuiting in that it will stop execution on the first
    argument that triggers a logical false result against the original predicates."
    ([p]
      (fn ep1
        ([] true)
        ([x] (boolean (p x)))
        ([x y] (boolean (and (p x) (p y))))
        ([x y z] (boolean (and (p x) (p y) (p z))))
        ([x y z & args] (boolean (and (ep1 x y z)
                                   (every? p args))))))
    ([p1 p2]
      (fn ep2
        ([] true)
        ([x] (boolean (and (p1 x) (p2 x))))
        ([x y] (boolean (and (p1 x) (p1 y) (p2 x) (p2 y))))
        ([x y z] (boolean (and (p1 x) (p1 y) (p1 z) (p2 x) (p2 y) (p2 z))))
        ([x y z & args] (boolean (and (ep2 x y z)
                                   (every? #(and (p1 %) (p2 %)) args))))))
    ([p1 p2 p3]
      (fn ep3
        ([] true)
        ([x] (boolean (and (p1 x) (p2 x) (p3 x))))
        ([x y] (boolean (and (p1 x) (p2 x) (p3 x) (p1 y) (p2 y) (p3 y))))
        ([x y z] (boolean (and (p1 x) (p2 x) (p3 x) (p1 y) (p2 y) (p3 y) (p1 z) (p2 z) (p3 z))))
        ([x y z & args] (boolean (and (ep3 x y z)
                                   (every? #(and (p1 %) (p2 %) (p3 %)) args))))))
    ([p1 p2 p3 & ps]
      (let [ps (list* p1 p2 p3 ps)]
        (fn epn
          ([] true)
          ([x] (every? #(% x) ps))
          ([x y] (every? #(and (% x) (% y)) ps))
          ([x y z] (every? #(and (% x) (% y) (% z)) ps))
          ([x y z & args] (boolean (and (epn x y z)
                                     (every? #(every? % args) ps)))))))))
