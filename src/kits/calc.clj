(ns ^{:doc "Converting numbers and assorted math."}
  kits.calc
  (:require [clojure.math.numeric-tower :refer [expt round]]
            [kits.map :refer [deep-merge-with]])
  (:import (java.math RoundingMode)
           (java.text NumberFormat)))

(def ^:const ZERO-STRING "0")
(def ^:const ONE-STRING "1")
(def ^:const INFINITE-STRING "INFINITE")
(def ^:const NOT-AVAILABLE "Not Available")

(defn format-to-string [big-decimal]
  (if (string? big-decimal)
    big-decimal
    (let [formatter (doto (NumberFormat/getNumberInstance)
                      (.setMaximumFractionDigits 2)
                      (.setMinimumFractionDigits 2))]
      (.format formatter big-decimal))))

(defn add-big-decimals [^BigDecimal bd1 ^BigDecimal bd2]
  (.add bd1 bd2))

(defn multiply-big-decimals [^BigDecimal bd1 ^BigDecimal bd2]
  (.multiply bd1 bd2))

(defn subtract-big-decimals [^BigDecimal bd1 ^BigDecimal bd2]
  (.subtract bd1 bd2))

(defn divide-big-decimals [^BigDecimal bd1 ^BigDecimal bd2]
  (.divide bd1 bd2 3 RoundingMode/HALF_UP))

(defn ^String remove-commas [^String s]
  (when s
    (.replaceAll ^String s "," "")))

(defn big-decimal-from-string [^String bd-string]
  (BigDecimal. (remove-commas (str bd-string))))

(defn double-from-string [^String double-string]
  (Double/parseDouble (remove-commas double-string)))

(defn big-decimal-less-than? [^BigDecimal bd1 ^BigDecimal bd2]
  (= -1 (.compareTo bd1 bd2)))

(defn big-decimal-greater-than? [^BigDecimal bd1 ^BigDecimal bd2]
  (= 1 (.compareTo bd1 bd2)))

(defn big-decimal-string-less-than? [bds-1 bds-2]
  (big-decimal-less-than? (big-decimal-from-string bds-1) (big-decimal-from-string bds-2)))

(defn big-decimal-string-greater-than? [bds-1 bds-2]
  (big-decimal-greater-than? (big-decimal-from-string bds-1) (big-decimal-from-string bds-2)))

(defn big-decimal-string-is-zero? [bds]
  (zero? (.compareTo (BigDecimal. "0") ^BigDecimal (big-decimal-from-string bds))))

(defn add-list-of-stringified-numbers [list-of-stringified-numbers]
  (let [bds (map big-decimal-from-string list-of-stringified-numbers)
        total (reduce add-big-decimals BigDecimal/ZERO bds)]
    (format-to-string total)))

(defn maybe-with [single-arg-fn arg id-value]
  (let [arg-to-use (if (and arg (not= "" arg)) arg id-value)]
    (single-arg-fn arg-to-use)))

(defn operate-on-big-decimal-strings [operation bds-1 bds-2 id-value]
  (if (or (= INFINITE-STRING bds-1) (= INFINITE-STRING bds-2))
    INFINITE-STRING
    (let [bd-1 (maybe-with big-decimal-from-string bds-1 id-value)
          bd-2 (maybe-with big-decimal-from-string bds-2 id-value)
          answer (operation bd-1 bd-2)]
      (format-to-string answer))))

(defn add-big-decimal-strings
  ([bds-1 bds-2]
     (operate-on-big-decimal-strings add-big-decimals bds-1 bds-2 ZERO-STRING))
  ([bds-1 bds-2 id-value]
     (operate-on-big-decimal-strings add-big-decimals bds-1 bds-2 id-value)))

(defn multiply-big-decimal-strings
  ([bds-1 bds-2]
     (multiply-big-decimal-strings bds-1 bds-2 ONE-STRING))
  ([bds-1 bds-2 id-value]
     (operate-on-big-decimal-strings multiply-big-decimals bds-1 bds-2 id-value)))

(defn subtract-big-decimal-strings
  ([bds-1 bds-2]
     (operate-on-big-decimal-strings subtract-big-decimals bds-1 bds-2 ZERO-STRING))
  ([bds-1 bds-2 id-value]
     (operate-on-big-decimal-strings subtract-big-decimals bds-1 bds-2 id-value)))

(defn divide-double-strings
  [ds1 ds2 precision]
  (format (str "%1." precision "f") (/ (double-from-string ds1) (double-from-string ds2))))

(defn divide-big-decimal-strings
  ([bds-1 bds-2]
     (divide-big-decimal-strings bds-1 bds-2 ONE-STRING))
  ([bds-1 bds-2 id-value]
     (if-not (seq bds-2)
       INFINITE-STRING
       (operate-on-big-decimal-strings divide-big-decimals bds-1 bds-2 id-value))))

(defn combine-with-operation [operation id-value acc-map single-map]
  (let [combiner (fn [val-1 val-2]
                   (operation val-1 val-2 id-value))]
    (reduce (fn [ret [k v]]
              (let [v-from-ret (ret k)]
                (assoc ret k (combiner v-from-ret v))))
            acc-map single-map)))

(defn combine-hashes-using-operation [list-of-hashes operation identity-value-for-operation]
  (let [combiner (fn [acc-map single-map]
                   (combine-with-operation operation identity-value-for-operation acc-map single-map))]
    (reduce combiner list-of-hashes)))

(defn combine-hashes-using-addition [list-of-hashes]
  (combine-hashes-using-operation list-of-hashes add-big-decimal-strings ZERO-STRING))

(defn combine-hashes-using-multiplication [list-of-hashes]
  (combine-hashes-using-operation list-of-hashes multiply-big-decimal-strings ONE-STRING))

(defn combine-hashes-using-subtraction [list-of-hashes]
  (combine-hashes-using-operation list-of-hashes subtract-big-decimal-strings ZERO-STRING))

(defn combine-hashes-using-division [list-of-hashes]
  (combine-hashes-using-operation list-of-hashes divide-big-decimal-strings ONE-STRING))

(defn big-decimal-string-scaler [scale]
  (let [scaler (big-decimal-from-string (str scale))]
    (fn [big-decimal-string]
      (multiply-big-decimal-strings scaler big-decimal-string ONE-STRING))))

(defn scale-hash-using [operator a-hash]
  (let [scaler (fn [ret [k v]] (assoc ret k (operator v)))]
    (reduce scaler a-hash a-hash)))

(defn round-2-decimal [number]
  (* 1.0 (/ (round (* number 100)) 100)))

(defn percentage [value percent]
  (round-2-decimal (* value (/ percent 100))))


(def PERCENT-SCALER (big-decimal-string-scaler 100))

(defn read-num
  "Read a number formatted with comas for easier reading of the report"
  [s]
  (cond
   (number? s) s
   (empty? s) nil
   :otherwise (Double/parseDouble (apply str (remove #{\,} s)))))

(defn do-safe
  "run a calculation which can throw an exception and has a fallback value"
  ([thunk]
     (do-safe thunk NOT-AVAILABLE))
  ([thunk fallback-value]
     (try
       (let [s (thunk)]
         (if (= s INFINITE-STRING)
           fallback-value
           s))
       (catch Exception e fallback-value))))

(defn safe-max [str-a str-b]
  (let [a (or (do-safe #(read-num str-a) 0) 0)
        b (or (do-safe #(read-num str-b) 0) 0)]
    (if (> a b)
      str-a
      str-b)))

(defn safe-read-num [a]
  (or (do-safe #(read-num (or a 0)) 0) 0))

(defn safe-read-num-positive [a]
  (let [safe-num (safe-read-num a)]
    (if (pos? safe-num) safe-num 0)))

(defn safe-op [a b op]
  (let [x (safe-read-num a)
        y (safe-read-num b)]
    (op x y)))

(defn safe-add
  ([]
     0)
  ([a]
     a)
  ([a b & cs]
     (apply safe-add (safe-op a b +) cs)))

(defn safe-subtract [a b]
  (safe-op a b -))

(defn safe-divide
  ([dividend divisor]
     (safe-divide dividend divisor 0.0))
  ([dividend divisor default]
     (try
       (let [result (/ (double dividend) divisor)]
         (if (or (Double/isNaN result)
                 (= Double/POSITIVE_INFINITY result))
           default
           result))
       (catch Exception e
         default))))


(defn cents->dollars
  ([c]
     (safe-divide c 100.0))
  ([c default]
     (safe-divide c 100.0 default)))


(defn round-places [number decimals]
  (let [factor (expt 10 decimals)]
    (bigdec (/ (round (* factor number)) factor))))

(defn nil-or-zero? [n]
  (or (nil? n)
      (zero? n)))

(defn nil-math [math-op & operands]
  (letfn [(nil->zero [n]
            (if (nil? n) 0.0 n))]
    (apply math-op (map nil->zero operands))))

(defn nil-long-math [math-op & operands]
  (letfn [(nil->zero [n]
            (if (nil? n) 0 n))]
    (apply math-op (map nil->zero operands))))

(defn calculate-discount-percentage [discount total-amount]
  (if (pos? total-amount)
    (* 100.0 (/ discount total-amount))
    0.0))

(defn as-percentage [n]
  (round-2-decimal (* 100 n)))

(defn merge-with-add [a b]
  (deep-merge-with (fn [a b]
                     (if (number? a)
                       (+ a b)
                       a))
                   a
                   b))

(defn double-abs [^Double num]
  (Math/abs num))

(defn ->dollars [cents]
  (-> cents cents->dollars round-2-decimal))

(defn ->percent [x]
  (as-percentage x))

(defn default-to-zero [x]
  (or x 0))
