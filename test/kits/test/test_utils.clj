(ns kits.test.test-utils
  (:use kits.test-utils
        clojure.test))

(deftest test-not-thrown?
  (is (not-thrown? Exception (+ 1 2))))

