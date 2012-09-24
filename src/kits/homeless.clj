(ns kits.homeless
  "Home of unsorted utility functions and macros."
  (:require
   [clojure.pprint :as pprint]
   [clojure.string :as str])
  (:import
   java.util.concurrent.Future
   java.util.concurrent.TimeoutException
   java.net.MalformedURLException))

(defmacro ignore-exceptions
  "Evaluate body, but return nil if any exceptions are thrown."
  [& body]
  `(try
     ~@body
     (catch Exception e# nil)))

(defmacro defn-cond
  "Variant of defn that allows for multiple alternative
  implementations in the body, one of which is used based on a
  matching predicate, e.g.,

  (defn-cond test [a b]
    (re-find #\"^1.2\" (clojure-version))
    (* a b)

    :else
    (+ a b))

  would define `test` one way under Clojure 1.2, and differently on
  other versions."
  [name & fdecl]
  (let [[m fdecl] (if (string? (first fdecl))
                    [{:doc (first fdecl)} (rest fdecl)]
                    [{} fdecl])
        [args & clauses] fdecl
        m (conj {:arglists (list 'list (list 'quote args))} m)]
    (list 'def
          (with-meta name m)
          (list*
           (reduce (fn [acc [pred body]]
                     (conj acc pred `(fn [~@args] ~body)))
                   ['cond]
                   (partition 2 clauses))))))

(defn raise
  "Raise a RuntimeException with specified message."
  [& msg]
  (throw (RuntimeException. ^String (apply str msg))))

(defn tap
  "Print the specified args, and return the value of the last arg."
  [& args]
  (apply println
         (cons "*** "
               (map #(if (string? %) % (with-out-str (pprint/pprint %)))
                    args)))
  (last args))

(defn parse-int
  "Parse integer value from string `s`"
  [s]
  (ignore-exceptions (Integer/parseInt s)))

(defn parse-long
  "Parse long integer value from string `s`"
  [s]
  (ignore-exceptions (Long/parseLong s)))

(defn parse-short
  "Parse short integer value from string `s`"
  [s]
  (ignore-exceptions (Short/parseShort s)))

(defn parse-float
  "Parse floating point value from string `s`"
  [s]
  (ignore-exceptions (Float/parseFloat s)))

(defn parse-double
  "Parse double precision number from string `s`"
  [s]
  (ignore-exceptions (Double/parseDouble s)))

(defn safely-read-string
  "Safer read-string."
  [s]
  (when s
    (read-string s)))

(defn parse-number
  "Parse a number from string `s`, optionally passing a default value
to return."
  ([s]
     (parse-number s nil))
  ([s default]
     (cond
       (number? s) s
       (empty? s)  default
       :else       (safely-read-string s))))

(defn rand-int*
  "Return a random integer between min (inclusive) and max (exclusive)."
  [min max]
  (+ min (rand-int (- max min))))

(defn time-ns
  "Current value of the most precise available system timer, in
  nanoseconds since epoch."
  []
  (System/nanoTime))

(defn time-us
  "Number of micro-seconds since epoch."
  []
  (long (/ (time-ns) 1000)))

(defn time-ms
  "Number of milli-seconds since epoch."
  []
  (System/currentTimeMillis))

(defn value-and-elapsed-time
  "Return the value of `thunk` and time taken to evaluate in
  microseconds." 
  [thunk]
  (let [start (time-us)
        value (thunk)]
    [value (- (time-us) start)]))

(defmacro bind-value-and-elapsed-time
  "Binds [value elapsed-time-us] from evaluating `expr` and invokes
  `body`."
  [bindings expr & body]
  `(let [~bindings (value-and-elapsed-time (fn [] ~expr))]
     ~@body))

(defn-cond call-with-timeout
  "Evaluate the function `f` but throw a RuntimeException if it takes
  longer than `timeout` milliseconds."
  [timeout-ms f]

  (re-find #"^1.2" (clojure-version))
  (let [^Future fut (future-call f)]
    (try
      (.get fut
            timeout-ms
            java.util.concurrent.TimeUnit/MILLISECONDS)
      (catch TimeoutException ex
        (future-cancel fut)
        (throw (RuntimeException. "Evaluation timeout")))))

  :else
  (let [ex (RuntimeException. "Evaluation timeout")
        fut (future-call f)
        r (deref fut timeout-ms ex)]
    (if (= ex r)
      (do
        (future-cancel fut)
        (throw ex))
      r)))

(defmacro with-timeout
  "Evaluate `body` but throw a RuntimeException if it takes longer
  than `timeout` milliseconds."
  [timeout & body]
  `(call-with-timeout ~timeout (bound-fn [] ~@body)))

(defn segregate
  "Returns [(filter f s) (remove f s)], only running through the seq once."
  [f s]
  (reduce (fn [[fl rl] i]
            (if (f i)
              [(conj fl i) rl]
              [fl (conj rl i)]))
          [(empty s) (empty s)]
          s))

(defn wrap-periodic
  "Add a wrapper fn, which executes `f` once every `period` calls."
  [f period]
  (let [count (atom 0)]
    (fn [& args]
      (swap! count inc)
      (when (zero? (mod count period))
        (apply f args)))))

(defn safely-sleep
  "Safely sleep for `ms` milliseconds."
  [ms]
  (try
    (Thread/sleep ms)
    (catch InterruptedException e
      (.interrupt ^Thread (Thread/currentThread)))))

(defn boolean?
  "Test if `x` is a boolean value."
  [x]
  (or (true? x) (false? x)))

(defn wrap-trapping-errors
  "Wraps the fn `f` to trap any Throwable, and return `default` in
   that case."
  [f & [default]]
  (fn [& args]
    (try
      (apply f args)
      (catch Throwable e default))))

(defn pos-integer?
  "Return true if `x` is a positive integer value."
  [x]
  (every? #(% x) [pos? integer?]))

(defn zero-or-pos-integer?
  "Return true if `x` is zero or positive integer value."
  [x]
  (or (zero? x) (pos-integer? x)))

(defn to-url
  "Returns a java.net.URL instance or nil if URL failed to parse"
  [^String s]
  (when s
    (try
      (java.net.URL. s)
      (catch MalformedURLException e
        nil))))

(def ^:private valid-ip-address-v4-re
  #"^([01]?\d\d?|2[0-4]\d|25[0-5])\.([01]?\d\d?|2[0-4]\d|25[0-5])\.([01]?\d\d?|2[0-4]\d|25[0-5])\.([01]?\d\d?|2[0-4]\d|25[0-5])$")

(defn ip-address-v4?
  "Test if the string `s` is a valid dotted IPv4 address."
  [s]
  (when s
    (boolean
     (re-matches valid-ip-address-v4-re s))))

