(ns kits.queues-test
  (:use
    clojure.test
    kits.foundation)
  (:require
    [kits.homeless :as kits]
    [kits.queues :as q]))

(deftest basic-queue-mechanics
  (let [q (q/create 1)]
    (is (true? (q/add q {:a 1})))
    (is (false? (q/add q {:b 2})))
    (is (false? (q/add q {:c 3})))
    (is (= {:a 1} (q/fetch q 0)))
    (is (true? (q/add q {:d 4})))
    (is (= {:d 4} (q/fetch q 0)))
    (is (nil? (q/fetch q 1)))
    (is (nil? (q/fetch q 1)))))

(deftest priority-queue
  (let [comparator (kits/make-comparator < :key-fn :id)
        q (q/create-with-priority comparator 3)]
    (is (true? (q/add q {:id 2 :name :foo})))
    (is (true? (q/add q {:id 1 :name :bar})))
    (is (= :bar (:name (q/peek q))))
    (is (= :bar (:name (q/fetch q 0))))
    (is (= :foo (:name (q/fetch q 0))))))
