(ns kits.file-test
  (:require [clojure.test :refer :all]
            [kits.file :refer :all]))

(set! *warn-on-reflection* false)


(deftest test-path
  (is (= "a/b/c" (path "a" "b" "c")))
  (is (= "/a/b/c" (path "/a" "b" "c")))
  (is (= "/a/b/c/" (path "/a" "b" "c/")))
  (is (= "/a/b/c/" (path "/a/" "/b/" "/c/")))
  (is (= "" (path)))
  (is (= "" (path nil))))
