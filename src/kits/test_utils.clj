(ns kits.test-utils
  "Functions and macros for making writing tests more effectively."
  (:require [clojure.data :as data]
            [clojure.pprint :as pprint])
  (:use clojure.test))

(set! *warn-on-reflection* false)

(defmacro assert-tables-equal
  "Assert that two seqs of seqs are equal, and when there is a
  failure, reports exactly which row/column pair failed."
  [expected-table actual-table]
  `(let [expected-table# ~expected-table
         actual-table# ~actual-table
         row-cnt# (count actual-table#)
         column-cnt# (count (first actual-table#))]

     (is (= row-cnt# (count expected-table#)))

     (doseq [row-idx# (range row-cnt#)
             column-idx# (range column-cnt#)]
       (is (= (get-in actual-table# [row-idx# column-idx#])
              (get-in expected-table# [row-idx# column-idx#]))
           (str "Row index: " row-idx# ", Column index: " column-idx#)))))


(defmethod clojure.test/assert-expr 'not-thrown? [msg form]
  ;; (is (not-thrown? c expr))
  ;; Asserts that evaluating expr does not throw an exception of class c.
  ;; Returns the exception thrown.
  (let [klass (second form)
        body (nthnext form 2)]
    `(try ~@body
          (do-report {:type :pass, :message ~msg,
                      :expected '~form, :actual nil})
          (catch ~klass e#
            (do-report {:type :fail, :message ~msg,
                        :expected '~form, :actual e#})
            e#))))


(defn diff
  [expected actual]
  (if-not (= expected actual)
    (let [[only-in-expected only-in-actual, in-both] (data/diff expected actual)]
      {:only-in-expected only-in-expected
       :only-in-actual only-in-actual})
    nil))

(defn ===
  "Compare 2 values with = and print diff on stdout when they do not
  match. Useful for friendly test failures."
  [expected actual]
  (if-let [delta (diff expected actual)]
    (do
      (pprint/pprint delta)
      false)
    true))

(defn open-port []
  (let [port (rand-nth (range 60000 65535))
        ss (try
             (java.net.ServerSocket. port)
             (catch Exception e nil))]
    (if ss
      (do (.close ss)  port)
      (open-port))))

(defn run-tests-with-spec [spec-meta spec f]
  (assert (even? (count spec)) "Spec needs to have even expected/args pair")
  (assert (:line spec-meta)
          "Need to pass in meta data of spec for test reporting\n.
           For example: pass the value of (meta #'spec) as spec-meta")
  (let [pairs (mapv #(vec %) (partition 2 spec))]
    (for [i (range (count pairs))
          :let [pair (nth pairs i)
                expected (first pair)
                actual (f (second pair))]
          :when (not= expected actual)]
      {:error (diff expected actual)
       :for (str expected " " (second pair))
       :at-line (+ (:line spec-meta) (inc i))
       :msg "If you see line number mismatch, make sure you do not have blank lines, extra comments lines and make sure first pair starts right after def spec line"})))

(defn error-ratio
  "Compute the error ratio for two floating-point values."
  [val1 val2]
  (let [dbl1 (double val1)
        dbl2 (double val2)
        abs-delta (Math/abs (- dbl1 dbl2))
        max-abs-val (Math/max (double (Math/abs dbl1))
                              (double (Math/abs dbl2)))]
    (/ abs-delta max-abs-val)))

(defmacro doseq-tests
  "Labels each case with an test-num starting from #0, and test failures print
  the right-hand side's binding values for that failure."
  [bindings & body]
  (let [syms-to-bind-to (first bindings)
        bindings-rhs-vec (vec (first (rest bindings)))]
    `(do
       ~@(for [i (range (count bindings-rhs-vec))
               :let [rhs (get bindings-rhs-vec i)]]
           `(let [~syms-to-bind-to ~rhs]
              (testing (str "\n   #" ~i ": " '~rhs)
                ~@body))))))
