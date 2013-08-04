(ns kits.test.flowcontrol
  (:require [clojure.test :refer :all]
            [kits.flowcontrol :refer :all]))


(deftest test-if-lets
  (is (= 3
         (if-lets [a true
                   b true]
                  (+ 1 2)
                  (+ 3 4))))

  (is (= 7
         (if-lets [a false
                   b true]
                  (+ 1 2)
                  (+ 3 4))))

  (is (= 7
         (if-lets [a true
                   b false]
                  (+ 1 2)
                  (+ 3 4))))
  
  (is (= 7
         (if-lets [a false
                   b false]
                  (+ 1 2)
                  (+ 3 4))))

  (is (= 3
         (if-lets [a true
                   b true]
                  (+ 1 2))))

  (is (= nil
         (if-lets [a false
                   b true]
                  (+ 1 2))))

  (is (= nil
         (if-lets [a true
                   b false]
                  (+ 1 2))))
  
  (is (= nil
         (if-lets [a false
                   b false]
                  (+ 1 2)))))


(deftest test-when-lets
  (is (= 3
         (when-lets [a true
                     b true]
                    :allows-implicit-do
                    (+ 1 2))))

  (is (= nil
         (when-lets [a false
                     b true]
                    (+ 1 2))))

  (is (= nil
         (when-lets [a true
                     b false]
                    (+ 1 2))))
  
  (is (= nil
         (when-lets [a false
                     b false]
                    (+ 1 2)))))
