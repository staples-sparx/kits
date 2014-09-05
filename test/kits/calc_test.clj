(ns kits.calc-test
  (:require [clojure.test :refer :all]
            [kits.calc :refer :all])) 

(deftest ^:unit test-combine-with-operation
  (let [am {:a "1" :b "2" :c "3" :d "4"}
        bm {:c "9" :d "16" :e "25"}
        combined (combine-with-operation add-big-decimal-strings "0" am bm)]
    (is (= (combined :a) "1"))
    (is (= (combined :b) "2"))
    (is (= (combined :c) "12.00"))
    (is (= (combined :d) "20.00"))
    (is (= (combined :e) "25.00"))))

(deftest ^:unit test-combine-hashes-using-operation
  (let [am {:a "1" :b "2" :c "3" :d "4"}
        bm {:c "9" :d "16" :e "25"}
        cm {:e "25" :f "10" :g "15"}
        dm {:f "3" :g "5" :h "7" :i "11" :j "13"}
        combined (combine-hashes-using-addition [am bm cm dm])]
    (is (= (combined :a) "1"))
    (is (= (combined :b) "2"))
    (is (= (combined :c) "12.00"))
    (is (= (combined :d) "20.00"))
    (is (= (combined :e) "50.00"))
    (is (= (combined :f) "13.00"))
    (is (= (combined :g) "20.00"))
    (is (= (combined :h) "7.00"))
    (is (= (combined :i) "11.00"))
    (is (= (combined :j) "13.00"))))

(deftest ^:unit test-maybe-usage-for-operations
  (is (= "-2.30" (subtract-big-decimal-strings nil "2.30" ZERO-STRING)))
  (is (= "2.30" (multiply-big-decimal-strings nil "2.30" ONE-STRING)))
  (is (= "2.30" (add-big-decimal-strings nil "2.30" ZERO-STRING)))
  (is (= "0.00" (divide-big-decimal-strings nil "2.30" ZERO-STRING)))
  (is (= INFINITE-STRING (divide-big-decimal-strings "2.30" nil ZERO-STRING))))

(deftest ^:unit test-alternate-signatures-on-math-operations
  (is (= "2.30" (add-big-decimal-strings "0.00" "2.30")))
  (is (= "0.00" (divide-big-decimal-strings "0.00" "2.30")))
  (is (= "2.30" (multiply-big-decimal-strings "1.00" "2.30")))
  (is (= "-2.30" (subtract-big-decimal-strings "0.00" "2.30" ZERO-STRING))))

(deftest ^:unit test-big-decimal-string-is-zero?
  (is (big-decimal-string-is-zero? "0.00"))
  (is (big-decimal-string-is-zero? "0"))
  (is (not (big-decimal-string-is-zero? "0.01")))
  (is (not (big-decimal-string-is-zero? "1")))
  (is (not (big-decimal-string-is-zero? "-0.01")))
  (is (not (big-decimal-string-is-zero? "-2"))))


(deftest ^:unit test-percentage
  (testing "should return 0 when value is 0"
    (is (= (percentage 0 50) 0.0)))
  (testing "should return 0 when percent is 0"
    (is (= (percentage 25 0) 0.0)))
  (testing "should round off to 2 digits"
    (is (= (percentage 90 50) 45.0))
    (is (= (percentage 4 3) 0.12))
    (is (= (percentage 4.1 3) 0.12))
    (is (= (percentage 4.25 3) 0.13)))
  (testing "should not have as ratio"
    (is (= (str (percentage 4 3)) "0.12"))))

(deftest ^:unit double-string-tests
  (is (= "0.33" (divide-double-strings "1.00" "3.00" 2)))
  (is (= "0.3333" (divide-double-strings "1.00" "3.00" 4)))
  (is (= 2000.0 (double-from-string "2,000.00")))
  (is (= 2000.0 (double-from-string "2000.00"))))

(deftest ^:unit test-nil-math
  (are [result args] (= result (apply (partial nil-math +) args))
       0 [0 0]
       1 [0 1]
       1.0 [nil 1]
       1.0 [1 nil]
       0.0 [nil nil]))

(deftest ^:unit test-calculate-discount-percentage
  (are [discount total-amount expected] (= expected (calculate-discount-percentage discount total-amount))
       10 100 10.0
       0 100 0.0
       10 0 0.0
       0 0 0.0))

(deftest ^:unit test-safe-add
  (is (= 0 (safe-add)))
  (is (= 3 (safe-add 3)))
  (is (= 10 (safe-add 3 7)))
  (is (= 21 (safe-add 3 7 11))))

(deftest ^:unit test-safe-divide
  (is (= 0.0 (safe-divide 100 0)))
  (is (= 0.0 (safe-divide 0 0)))
  (is (= 2.0 (safe-divide 100 50)))

  (is (= 0.0 (safe-divide 100.0 0.0)))
  (is (= 0.0 (safe-divide 0.0 0.0)))
  (is (= 2.0 (safe-divide 100.0 50.0))))
