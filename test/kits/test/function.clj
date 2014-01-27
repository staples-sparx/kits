(ns kits.test.function
  (:require [clojure.test :refer :all]
            [kits.function :refer :all]))


(deftest test-flip
  (is (= [1 2 3 4]
         ((flip conj) 4 [1 2 3]))))
