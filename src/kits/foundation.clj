(ns kits.foundation
  (:require
   [clojure.pprint :as pprint]
   [clojure.string :as str])
  (:import
   java.util.concurrent.Future
   java.util.concurrent.TimeoutException))

(defn raise
  "Raise a RuntimeException with specified message."
  [& msg]
  (throw (RuntimeException. ^String (apply str msg))))

(defmacro ignore-exceptions [& body]
  `(try
     ~@body
     (catch Exception e# nil)))

(defn parse-int [str] (ignore-exceptions (Integer/parseInt str)))
(defn parse-long [str] (ignore-exceptions (Long/parseLong str)))
(defn parse-short [str] (ignore-exceptions (Short/parseShort str)))
(defn parse-float [str] (ignore-exceptions (Float/parseFloat str)))
(defn parse-double [str] (ignore-exceptions (Double/parseDouble str)))

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

(defn tap [& args]
  (apply println
         (cons "*** "
               (map #(if (string? %) % (with-out-str (pprint/pprint %)))
                    args)))
  (last args))

(defn nano-time
  "Returns the current value of the most precise available system
timer, in nanoseconds."
  []
  (System/nanoTime))

(defn micro-time
  "Return the number of micro-seconds since the epoch."
  []
  (long (/ (nano-time) 1000)))

(defn ms-time
  "Return the number of milli-seconds since the epoch."
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
  (let [[b1 b2 b3 b4] (map #(or (parse-int %)
                                (raise (format "Invalid IP address: %s" dotted)))
                           (str/split dotted #"\."))]
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

(defn uuid
  "Return a UUID string."
  []
  (str (java.util.UUID/randomUUID)))

(defmacro do1
  "Evaluate expr and exprs and return the value of expr."
  [expr & exprs]
  `(let [v# ~expr]
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
         (.delete ~var)))))

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
