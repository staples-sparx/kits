(ns kits.test.queues
  (:use
   clojure.test)
  (:require
   [kits.queues :as q]))

(deftest basic-queue-mechanics
  (let [q (q/create 1)]
    (is (true? (q/add q {:a 1})))
    (is (false? (q/add q {:b 2})))
    (is (false? (q/add q {:c 3})))
    (is (= {:a 1} (q/fetch q 0)))
    (is (true? (q/add q {:d 4})))
    (is (= {:d 4} (q/fetch q 0)))
    (is (= nil (q/fetch q 1)))
    (is (= nil (q/fetch q 1)))
    ))
