(ns kits.test.homeless
  (:use clojure.test)
  (:require [kits.homeless :as h]))

(deftest raise
  (is (thrown? RuntimeException (h/raise "test exception"))))

(deftest parse-numbers
  "Tests parse-int, parse-long, parse-short, parse-float, and parse-double"
  (is (nil? (h/parse-int "foo")))
  (is (nil? (h/parse-long nil)))
  (is (= 1 (h/parse-int "1")))
  (is (= 1.0 (h/parse-double "1.0")))
  (is (= 1.0 (h/parse-float "1.0")))

)

(deftest parse-number
  (are [expected str default] (= expected (h/parse-number str default))
    nil nil nil
    10 "10" nil
    nil "" nil
    24 "" 24
    10.0 "10.00" nil
    10.0 "10.00" 0
    10 10 0))

(deftest value-and-elapsed-time

  (h/bind-value-and-elapsed-time [val elapsed]
      (+ 1 1)

    (is (= 2 val))

    (is (number? elapsed))))

(deftest segregate
  (is (= [["a" "b"] [1 2]] (h/segregate string? [1 "a" "b" 2])))
  (is (= [nil nil] (h/segregate string? nil)))
  (is (= [[] []] (h/segregate string? []))))

(deftest boolean?
  (are [x bool?] (= bool? (h/boolean? x))
    false true
    true true
    nil  false
    "ad" false
    []   false
    {}   false))

(deftest wrap-trapping-errors
  (is (false? ((h/wrap-trapping-errors pos? false) "string")))
  (is (nil? ((h/wrap-trapping-errors pos?) "string")))
  (is (true?  ((h/wrap-trapping-errors string?) "string")))
)







