(ns kits.core
  (:use clojure.pprint)
  (:require [clojure.string :as str]
            [clojure.java.shell :as sh]
            [kits.foundation :as f]
            [kits.map :as m])
  (:import [java.io File FileOutputStream]))


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
      (into (sorted-map) (m/map-values nested-sort x))
      (m/map-values nested-sort x))

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
