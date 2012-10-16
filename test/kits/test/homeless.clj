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
  (is (true?  ((h/wrap-trapping-errors string?) "string"))))

(deftest ip-address-v4?
  (is (false? (h/ip-address-v4? "34.342.3.4")))
  (is (h/ip-address-v4? "192.168.0.1")))

(deftest str->boolean
  (is (h/str->boolean "true"))
  (is (false? (h/str->boolean "")))
  (is (false? (h/str->boolean "false"))))

(deftest base-array?
  (is (h/base-array? (to-array '(1 2 2 :c :d :e))))
  (is (h/base-array? (into-array {:a 3})))
  (is (h/base-array? (to-array [:a 3])))
  (is (false? (h/base-array? '(1 2 2 :c :d :e))))
)

(deftest seq-to-map
  (is (= {:a 2, :b 4, :c 5} (h/seq-to-map '([:a 2] [:b 4] [:c 5]))))
  (is (nil? (h/seq-to-map nil)))
  (is (nil? (h/seq-to-map '()))))

(deftest ipv4-dotted-to-integer
  (is (= (h/ipv4-dotted-to-integer "127.0.0.1") 2130706433)))

(deftest ipv4-integer-to-dotted
  (is (= "127.0.0.1" (h/ipv4-integer-to-dotted (h/ipv4-dotted-to-integer "127.0.0.1")))))

(deftest parse-url
  (is (= {:scheme "http", :host "www.runa.com", :path "/design"} (h/parse-url "http://www.runa.com/design")))
  (is (= nil (h/parse-url "")))
  (is (= nil (h/parse-url nil)))
)

(deftest rmerge
  (is (= {:a 1 :b 2} (h/rmerge {:b 2} {:a 1})))
  (is (= {:a {:x {:y {:z 3}}} :b 2} (h/rmerge {:a {:x 1} :b 2} {:a {:x {:y {:z 3}}}})))
  (is (= {:a {:x {:y {:z 3}}} :b 2} (h/rmerge {:a {:x {:y {:z 1}}} :b 2} {:a {:x {:y {:z 3}}}})))
)

(deftest url?
  (is (false? (h/url? "malformedhttp:// url")))
  (is (false? (h/url? "")))
  (is (false? (h/url? nil)))
  (is (h/url? "http://www.runa.com/")))

(deftest zip
  (are [lists result] (= (h/zip lists) result)
    nil []
    []  []
    [[:a 1 \x]]             [[:a] [1] [\x]]
    [[:a] [1] [\x]]         [[:a 1 \x]] ;; reversible!
    [[:a 1 \x] [:b 2 \y]]   [[:a :b] [1 2] [\x \y]]
    [[:a :b] [1 2] [\x \y]] [[:a 1 \x] [:b 2 \y]])) ;; reversible!

(deftest nested-sort
  (are [input sorted] (= sorted (h/nested-sort input))
    {} {}
    [] []
    nil nil
    #{} #{}
    [[3 2 1]] '((1 2 3))))

(deftest indexed
  (is (= '([0 a] [1 b] [2 c] [3 d]) (h/indexed '(a b c d))))
  (is (= '() (h/indexed 'nil))
  (is (= '() (h/indexed '()))))
)
