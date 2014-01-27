(ns kits.test.test-utils
  (:use clojure.test
        kits.test-utils))

(deftest test-not-thrown?
  (is (not-thrown? Exception (+ 1 2))))

(deftest test===
  (is (= {:only-in-expected {:b 2}, :only-in-actual {:b 3}}
         (diff  {:a 1 :b 2}
                {:a 1 :b 3}))))
