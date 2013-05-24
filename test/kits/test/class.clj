(ns kits.test.class
  (:use clojure.test)
  (:require [kits.class :as cl]))

(deftest is-a-base-array
  (testing "Determince if a given object is Java array"
    (is (cl/base-array? (to-array '(1 2 2 :c :d :e))))
    (is (cl/base-array? (into-array {:a 3})))
    (is (cl/base-array? (to-array [:a 3])))
    (is (false? (cl/base-array? '(1 2 2 :c :d :e))))
    (is (cl/base-array? (make-array Double/TYPE 3)))
    (is (false? (cl/base-array? (doto (java.util.ArrayList.)
                                  (.add "test")))))))

(deftest safe-cast
  (testing "Given a class and object, determines if object is an instance of the class"
    (is (nil? (cl/safe-cast clojure.lang.PersistentTreeMap (hash-map :test 1))))
    (is (= {:test 1}
           (cl/safe-cast clojure.lang.PersistentTreeMap (sorted-map :test 1))))))
