(ns kits.test-utils
  (:use clojure.test))


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


;; TODO - Alex - Nov 26, 2012 - taken from furtive.test.unit.spec-utils.spec-utils
;; Replace uses of it in Furtive with this.  And I'm sure there are
;; plenty of other good spec-utils to pull into this namespace as well.
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
