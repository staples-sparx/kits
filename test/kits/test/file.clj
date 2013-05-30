(ns kits.test.file
  (:require [clojure.test :refer :all]
            [kits.file :refer :all]))


(deftest test-path
  (is (= "a/b/c" (path "a" "b" "c")))
  (is (= "/a/b/c" (path "/a" "b" "c")))
  (is (= "/a/b/c/" (path "/a" "b" "c/")))
  (is (= "/a/b/c/" (path "/a/" "/b/" "/c/")))
  (is (= "" (path)))
  (is (= "" (path nil))))