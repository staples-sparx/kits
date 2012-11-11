(ns kits.homeless
  "Unfortunate, uncategorized utility functions and macros. Please
   help one of these poor souls find a home :("
  (:require
   [clojure.pprint :as pprint]
   [clojure.string :as str]
   [kits.map :as m])
  (:import
   java.util.concurrent.Future
   java.util.concurrent.TimeoutException
   java.io.File
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

(defn- time-elapsed*
  "Returns time elapsed in millis."
  [f]
  (let [start (System/nanoTime)]
    (f)
    (/ (double (- (System/nanoTime) start)) 1000000.0)))

(defmacro time-elapsed
  "Returns time elapsed in millis."
  [& body]
  `(time-elapsed* (fn [] ~@body)))

(defn raise
  "Raise a RuntimeException with specified message."
  [& msg]
  (throw (RuntimeException. ^String (apply str msg))))

(defn print-vals
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

(defn read-string-safely [s] (when s (read-string s)))

(defn parse-number
  "Parse a number from string `s`, optionally passing a default value
to return."
  ([s]
     (parse-number s nil))
  ([s default]
     (cond
       (number? s) s
       (empty? s)  default
       :else       (read-string-safely s))))

(defn rand-int*
  "Return a random integer between min (inclusive) and max (exclusive)."
  [min max]
  (+ min (rand-int (- max min))))

(defn- time-ns
  "Current value of the most precise available system timer, in
  nanoseconds. This is NOT a guaranteed absolute time like time-ms and
  doesn't work the same across all JVM architectures. Use this for
  measuring TIME INTERVALS ONLY. See javadoc for System.nanoTime() for
  more details."
  []
  (System/nanoTime))

(defn- time-us
  "Number of micro-seconds since epoch. This is NOT a guaranteed
  absolute time like time-ms and doesn't work the same across all
  architectures. Use this for measuring TIME INTERVALS ONLY. See
  javadoc for System.nanoTime() for more details."
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

(defmacro periodic-fn
  "creates a fn that executes 'body' every 'period' calls"
  [args [var period] & body]
  `(let [call-count# (atom 0)]
     (fn [~@args]
       (swap! call-count# inc)
       (when (zero? (mod @call-count# ~period))
         (let [~var @call-count#]
           ~@body)))))

(defn wrap-periodic
  "Returns a fn which wraps f, that executes `f` once every `period` calls."
  [f period]
  (let [count (atom 0)]
    (fn [& args]
      (swap! count inc)
      (when (zero? (mod @count period))
        (apply f args)))))

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

(defn url? [s]
  (boolean (to-url s)))

(def ^:private valid-ip-address-v4-re
  #"^([01]?\d\d?|2[0-4]\d|25[0-5])\.([01]?\d\d?|2[0-4]\d|25[0-5])\.([01]?\d\d?|2[0-4]\d|25[0-5])\.([01]?\d\d?|2[0-4]\d|25[0-5])$")

(defn ip-address-v4?
  "Test if the string `s` is a valid dotted IPv4 address."
  [s]
  (when s
    (boolean
     (re-matches valid-ip-address-v4-re s))))

(defn str->boolean
  "Boolean value for the specified string, per the following rules:

  \"true\" => true
  \"false\" => false
  \"foobar\" => true
  nil or \"\" => false"
  [^String s]
  (if (not-empty s) (not= "false" (.toLowerCase s)) false))

(defn base-array?
  "Test if specified array is of a base-type (long/double etc.)"
  [a]
  (and (or a false) (.isArray ^Class (class a))))

(defn fprint
  "Same as print but explicitly flushes *out*."
  [& more]
  (apply print more)
  (flush))

(defn fprintln
  "Same as println but explicitly flushes *out*."
  [& more]
  (apply println more)
  (flush))

;; progress reporting

(def ^{:dynamic true} *print-progress* true)

(defn make-default-progress-reporter
  "A basic progress reporter function which can be used with
  `with-progress-reporting`."
  [{:keys [iters-per-row num-columns row-handler row-fmt no-summary]}]
  (let [iters-per-row (or iters-per-row 1000)
        num-columns (or num-columns 60)
        iters-per-dot (int (/ iters-per-row num-columns))
        row-handler (fn [i]
                      (if row-handler
                        (str " " (row-handler i))
                        ""))
        row-fmt (or row-fmt "%,8d rows%s")]
    (fn [i final?]
      (cond
        final?
        (when-not no-summary
          (fprintln (format row-fmt i (row-handler i))))

        (zero? (mod i iters-per-row))
        (fprintln (format row-fmt i (row-handler i)))

        (zero? (mod i iters-per-dot))
        (fprint ".")))))

(defmacro with-progress-reporting
  "Bind a `reportfn` function, and evaluate `body` wherein
  calling (report!) will invoke the report function with the current
  state of the iteration."
  [opts & body]
  `(let [iter# (atom 0)
         opts# (or ~opts {})
         reporter# (or (:reporter opts#)
                       (make-default-progress-reporter opts#))]
     (letfn [(report# [& [fin?#]]
               (when *print-progress*
                 (when-not fin?# (swap! iter# inc))
                 (reporter# @iter# (boolean fin?#))))]
       (let [~'report! report#
             val# (do ~@body)]
         (report# true)
         val#))))

(defn seq-to-map
  "Transforms a seq of ([key1 value1] [key2 value2]) pairs to a map
  {key1 value1 key2 value2}. For empty and nil values, returns nil."
  [coll]
  (when (seq coll)
    (into {} coll)))

(defn ipv4-dotted-to-integer
  "Convert a dotted notation IPv4 address string to a 32-bit integer.

  (ipv4-dotted-to-integer \"127.0.0.1\")
  => 2130706433"
  [dotted]
  (let [[b1 b2 b3 b4] (map #(or (parse-int %)
                                (raise (format "Invalid IP address: %s" dotted)))
                           (str/split dotted #"\."))]
    (bit-or (bit-or (bit-or (bit-shift-left b1 24)
                            (bit-shift-left b2 16))
                    (bit-shift-left b3 8))
            b4)))

(defn ipv4-integer-to-dotted
  "Convert a 32-bit integer into a dotted notation IPv4 address string.

  (ipv4-integer-to-dotted (ipv4-dotted-to-integer \"127.0.0.1\"))
  => \"127.0.0.1\""
  [ip]
  (format "%d.%d.%d.%d"
          (bit-and (bit-shift-right ip 24) 0xff)
          (bit-and (bit-shift-right ip 16) 0xff)
          (bit-and (bit-shift-right ip 8) 0xff)
          (bit-and ip 0xff)))

(defn uuid
  "Return a UUID string."
  []
  (str (java.util.UUID/randomUUID)))

(defmacro do-all-return-first
  "Evaluate expr1 and exprs and return the value of expr1."
  [expr1 & exprs]
  `(let [v# ~expr1]
     ~@exprs
     v#))

(defmacro with-temp-file
  "Bind var to a temp File instance and invoke body, and delete the
  file on return."
  [var & body]
  `(let [~var (io/file (str "/tmp/" (uuid)))]
     (try
       (do ~@body)
       (finally
         (when (.exists ~var)
           (.delete ~var))))))

(defn parse-url
  "Parse the url spec into a map with keys {:scheme, :host, etc.}"
  [^String spec]
  (when (seq spec)
    (try
      (let [[scheme comps] (if (re-find #".*://" spec)
                             (str/split spec #"://")
                             ["file" spec])
            [raw-host raw-path] (let [[h & r] (str/split comps #"/")]
                                  [h (str "/" (str/join "/" r))])
            comps (str/split raw-host #"@")
            host (last comps)
            [username password] (if (< 1 (count comps))
                                  (str/split (first comps) #":")
                                  [nil nil])
            [path & [query]] (str/split raw-path #"\?")]
        (into {}
              (filter val
                      {:scheme scheme
                       :username (not-empty username)
                       :password (not-empty password)
                       :host (not-empty host)
                       :path (not-empty path)
                       :query (not-empty query)})))
      (catch Exception ex
        nil))))

(defn rmerge
  "Recursive merge of the provided maps."
  [& maps]
  (if (every? map? maps)
    (apply merge-with rmerge maps)
    (last maps)))

(defn print-error
  "Println to *err*"
  [& args]
  (binding [*out* *err*]
    (apply println args)))

(defn safe-sleep
  "Sleep for `millis` milliseconds."
  [millis]
  (try (Thread/sleep millis)
    (catch InterruptedException e
      (.interrupt ^Thread (Thread/currentThread)))))

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

(def p (comp pprint/pprint nested-sort))

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

(defn incremental-name-with-prefix [prefix]
  (let [cnt (atom -1)]
    (fn []
      (swap! cnt inc)
      (str prefix "-" @cnt))))

(defn ensure-sequential [x]
  (if (sequential? x)
    x
    [x]))

(defn retrying-fn
  "Take a no-arg function f and max times it should be called, returns a new no-arg
 function that will call f again if calling f throws a Throwable."
  [f max-times]
  (fn this
    ([]
      (this max-times))
    ([retry-count]
      (try
        (f)
        (catch Throwable t
          (if (zero? retry-count)
            (throw t)
            (this (dec retry-count))))))))

(defmacro with-retries [retry-count & body]
  `((retrying-fn
      (fn [] ~@body) ~retry-count)))

(defn any? [pred coll]
  (boolean (some pred coll)))

(defn butlastv
  "Like (vec (butlast v))' but efficient for vectors"
  [v]
  (let [cnt (count v)]
    (if (< cnt 2)
      []
      (subvec v 0 (dec cnt)))))

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
