(ns kits.test.xml
  (:use clojure.test
        kits.xml))

(deftest test-content-of-node-with-tag
  (is (= ["Gomez"]
         (content-of-node-with-tag [{:tag :name
                                     :content [{:tag :last
                                                :content ["Gomez"]}]}]
           :name :last))))

(deftest test-parse
  (is (= {:tag :foo
          :attrs nil
          :content ["bar"]}
         (parse "<foo>bar</foo>"))))