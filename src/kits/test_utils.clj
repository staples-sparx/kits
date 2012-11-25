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
