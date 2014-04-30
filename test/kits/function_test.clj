(ns kits.function-test
  (:require [clojure.test :refer :all]
            [kits.function :refer :all]))

(set! *warn-on-reflection* false)


(deftest test-flip
  (is (= [1 2 3 4]
         ((flip conj) 4 [1 2 3]))))
