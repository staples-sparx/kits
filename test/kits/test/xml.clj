(ns kits.xml
  (:use clojure.test
        groupon.xml))

(deftest test-content-of-node-with-tag
  (is (= ["Gomez"]
         (content-of-node-with-tag [{:tag :name
                                     :content [{:tag :last
                                                :content ["Gomez"]}]}]
           :name :last))))