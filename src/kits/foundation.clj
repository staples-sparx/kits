(ns ^{:doc "Basic clojure helper functions"}
  kits.foundation
  (:require
    [clojure.pprint :as pprint]
    [clojure.string :as str]
    [clojure.data :as data])
  (:import
    java.util.concurrent.Future
    java.util.concurrent.TimeoutException
    java.io.StringWriter
    java.util.UUID
    java.security.MessageDigest
    java.security.NoSuchAlgorithmException))

(set! *warn-on-reflection* true)

(defmacro _+ [a b] `(unchecked-add (long ~a) (long ~b)))
(defmacro _- [a b] `(unchecked-subtract (long ~a) (long ~b)))
(defmacro _* [a b] `(long (unchecked-multiply (long ~a) (long ~b))))
(defmacro _div [a b] `(long (/ (long ~a) (long ~b))))
(defmacro _rem [a b] `(let [a# (long ~a)
                            b# (long ~b)]
                        (- a# (_* (long (_div a# b#)) b#))))
(defmacro _1+ [a] `(unchecked-inc (long ~a)))
(defmacro _1- [a] `(unchecked-dec (long ~a)))

(defn tap [& args]
  (apply println
    (cons "*** "
      (map #(if (string? %)
              %
              (with-out-str (pprint/pprint %)))
        args)))
  (last args))

(defn within?
  ([expected actual]
     (within? expected actual 0.01))
  ([expected actual delta]
     (<= (Math/abs (float (- expected actual))) delta)))

(defn nano-time
  "Returns the current value of the most precise available system timer, in nanoseconds."
  []
  ;; http://www.javacodegeeks.com/2012/02/what-is-behind-systemnanotime.html
  ;;
  ;; ph7 - Returned values are very different on OS X, Linux, Solaris
  ;;
  ;; On Linux:
  ;;
  ;;     user=> (System/currentTimeMillis)
  ;;     1354659982789
  ;;     user=> (System/nanoTime)
  ;;     88526629023881
  ;;
  ;; On OS X:
  ;;
  ;;    user> (System/currentTimeMillis)
  ;;    1354660113914
  ;;    user> (kits.foundation/nano-time)
  ;;    1354660119443686000
  ;;
  ;; As a consequence I cannot compute absolute time/data based on (System/nanoTime)
  ;;
  ;; Another problem is that the value can leap back or just forward.
  ;; It may not change monotonically and change rate can vary with
  ;; dependency on CPU clock speed.
  ;;
  ;; If there are multiple cpus/cores, the running thread seem
  ;; to switch between different cpus and each cpu seem to have a different
  ;; timer base the result of nanoTime is jumping forward and backward in
  ;; time, depending on which cpu the thread is currently running.
  ;;
  ;; On Linux the value is read from clock_gettime with CLOCK_MONOTONIC flag
  ;; (for real man, source is available in vclock_gettime.c from Linux source). Which uses
  ;; either TSC or HPET. The only difference with Windows is that Linux not even trying
  ;; to sync values of TSC read from different CPUs, it just returns it as it is. It
  ;; means that value can leap back and jump forward with dependency of CPU where it is
  ;; read. Also, in contract to Windows, Linux doesn’t keep change frequency constant.
  ;; On the other hand, it definitely should improve performance.
  ;;

  ;; ph7 - work around the problem for now (System/nanoTime)
  (_* (System/currentTimeMillis) 1000000)
  )

(defn nano->seconds [ts]
  (_div (long ts) (long 1000000)))

(defn micro->milli [ts]
  (_div (long ts) (long 1000)))

(defn nano->milli [ts]
  (_div (long ts) (long 1000000)))

^{:static true}
(defn nano->min ^long [^long ts]
  (/ ts (long 60000000000)))

(defn min->nano [ts]
  (_* (long ts) (long 60000000000)))

(defn min->ms [ts]
  (_* (long ts) (long 60000)))

(defn ms->min [ts]
  (_div (long ts) (long 60000)))

(defn ms-time
  " Returns number of milli-seconds since the epoch"
  []
  (System/currentTimeMillis))

(defn starts-with [^String prefix ^String s]
  (.startsWith s prefix))

(defn ends-with [^String suffix ^String s]
  (.endsWith s suffix))

(defn substring? [^String fragment ^String str]
  (>= (.indexOf str fragment) 0))

(defn last-index-of [^String fragment ^String str]
  (.lastIndexOf str fragment))

(defn raise
  "Raise a RuntimeException with specified message."
  [& msg]
  (throw (RuntimeException. ^String (apply str msg))))

(defn split [^String str ^String sep]
  (.split str sep))

(defn trim [^String str]
  (when str
    (.trim str)))

(defn blank? [^String str]
  (or (nil? str) (= "" str)))

(defn squeeze
  "Remove any space character in a string"
  [^String str]
  (.replaceAll str "\\s+" ""))

(defn strip-new-lines [^String str]
  (.replaceAll str "[\r\n]+" ""))

(defn str-replace [^String regex ^String replacement ^String str]
  (when str
    (.replaceAll str regex replacement)))

(defn lowercase [str]
  (.toLowerCase ^String str))

(defn truncate-str [^Integer len ^String s]
  (when s
    (if (> (.length s) len)
      (.substring s 0 len)
      s)))

(defn print-error [& args]
  "Println to *err*"
  (binding [*out* *err*]
    (apply println args)))

(defmacro blindly [ & body]
  `(try
     ~@body
     (catch Exception e#
       (print-error "Ignoring error " e#))))

(defmacro nil-on-exceptions [& body]
  `(try
     ~@body
     (catch Exception e#
       nil)))

(defn parse-int [str]
  (nil-on-exceptions
    (Integer/parseInt str)))

(defn parse-long [str]
  (nil-on-exceptions
    (Long/parseLong str)))

(defn parse-short [str]
  (nil-on-exceptions
    (Short/parseShort str)))

(defn parse-float [str]
  (nil-on-exceptions
    (Float/parseFloat str)))

(defn parse-double [str]
  (nil-on-exceptions
    (Double/parseDouble str)))

(defn parse-boolean [str]
  (Boolean/parseBoolean str))

(defn long? [n]
  (or
   (= Long (class n))))

(defn truncate-after-first-decimal [n]
  (float (/ (int  (Math/round (float (* (float n) 10.0)))) 10)))

(defn update-keys
  "Apply a function on all keys of a map and return the corresponding map (all values untouched)"
  [f a-map]
  (zipmap
    (map f (keys a-map))
    (vals a-map)))

(defn update-vals
  "Apply a function on all values of a map and return the corresponding map (all keys untouched)"
  [f a-map]
  (zipmap
    (keys a-map)
    (map f (vals a-map))))

(defn dissoc-in [m & keys-seq]
  (reduce
    (fn [m keys]
      (let [last (last keys)
            prefix (drop-last keys)]
        (if (= 0 (count prefix))
          (dissoc m last)
          (update-in m
            prefix
            #(dissoc % last)))))
    m
    keys-seq))

(defn diff
  [expected actual]
  (if-not (= expected actual)
    (let [[only-in-expected only-in-actual, in-both] (data/diff expected actual)]
      {:only-in-expected only-in-expected
       :only-in-actual only-in-actual})
    nil))

(defn ===
  "Compare 2 values with = and print diff on stdout when they do not match. Useful for friendly test failures."
  [expected actual]
  (if-let [delta (diff expected actual)]
    (do
      (pprint/pprint delta)
      false)
    true))

(defn deep-merge [& maps]
  (apply
    (fn merge-one-level [& maps]
      (if (every? map? maps)
        (apply merge-with merge-one-level maps)
        (last maps)))
    maps))

(defn uuid
  "Return a UUID string."
  []
  (str (UUID/randomUUID)))

(defn str->boolean
  "Boolean value for the specified string, per the following rules:

  \"true\" => true
  \"false\" => false
  \"foobar\" => true
  nil or \"\" => false"
  [^String s]
  (if (not-empty s) (not= "false" (.toLowerCase s)) false))

(defn csv-string->keyword-set
  "Parses comma seperated string into keyword set"
  [csv-string]
  (into #{} (map keyword (str/split csv-string #","))))

(defn base-array?
  "Test if specified array is of a base-type (long/double etc.)"
  [a]
  (and (or a false) (.isArray ^Class (class a))))

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

(def ^:dynamic *print-progress* true)

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
  {key1 value1 key2 value2}."
  [s]
  (if (seq s)
    (apply assoc {} s)
    {}))

(defn dotted-to-ip
  "Convert a dotted notation IPv4 address string to a 32-bit integer.
  (dotted-to-ip \"127.0.0.1\")
  => 2130706433"
  [dotted]
  (let [[b1 b2 b3 b4] (map #(Integer/parseInt ^String %)
                           (.split ^String dotted "\\."))]
    (bit-or (bit-or (bit-or (bit-shift-left b1 24)
                            (bit-shift-left b2 16))
                    (bit-shift-left b3 8))
            b4)))

;; (= (dotted-to-ip "127.0.0.1") 2130706433)

(defn ip-to-dotted
  "Convert a 32-bit integer into a dotted notation IPv4 address string.

  (ip-to-dotted (dotted-to-ip \"127.0.0.1\"))
  => \"127.0.0.1\""
  [ip]
  (format "%d.%d.%d.%d"
          (bit-and (bit-shift-right ip 24) 0xff)
          (bit-and (bit-shift-right ip 16) 0xff)
          (bit-and (bit-shift-right ip 8) 0xff)
          (bit-and ip 0xff)))

;; (ip-to-dotted (dotted-to-ip "127.0.0.1"))


(defmacro do1
  "Evaluate expr1 and exprs and return the value of expr1."
  [expr1 & exprs]
  `(let [v# ~expr1]
     ~@exprs
     v#))

(defn parse-url
  "Parse the url spec into a map with keys {:scheme, :host, etc.}"
  [^String spec]
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
      nil)))

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

(defn rmerge
  "Recursive merge of the provided maps."
  [& maps]
  (if (every? map? maps)
    (apply merge-with rmerge maps)
    (last maps)))

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
