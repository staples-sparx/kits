(ns kits.queues-test
  (:use clojure.test)
  (:require [kits.homeless :as kits]
            [kits.queues :as q]))

(set! *warn-on-reflection* false)

(deftest basic-queue-mechanics
  (let [q (q/make-basic-queue 1)]
    (is (true? (q/offer! q {:a 1})))
    (is (false? (q/offer! q {:b 2})))
    (is (false? (q/offer! q {:c 3})))
    (is (= {:a 1} (q/poll! q 0)))
    (is (true? (q/offer! q {:d 4})))
    (is (= {:d 4} (q/poll! q 0)))
    (is (nil? (q/poll! q 1)))
    (is (nil? (q/poll! q 1)))))

(def test-comparator (kits/make-comparator < :key-fn :id))

(deftest priority-queue
  (let [q (q/make-priority-queue test-comparator)]
    (is (true? (q/offer! q {:id 2 :name :foo})))
    (is (true? (q/offer! q {:id 1 :name :bar})))
    (is (= :bar (:name (q/peek q))))
    (is (= :bar (:name (q/poll! q))))
    (is (= :foo (:name (q/poll! q))))))
