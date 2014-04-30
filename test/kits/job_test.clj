(ns kits.job-test
  (:require [clojure.test :refer :all]
            [kits.job :refer :all]
            [kits.logging.log-async :as log]))

(set! *warn-on-reflection* false)

(defn- successful-inc [val job]
  [(inc val) job])

(defn- failing-fn [val job]
  [val (abort-job job "Cannot succeed!")])

(defn- exception-fn [val job]
  (throw (Exception. "foobar")))

(deftest job-test
  (log/reset-q! 100)

  (testing "when run successfully"
    (let [[result job] (run-with-short-circuiting (create-job) 0 (repeat 5 successful-inc))]
      (is (= 5 result))
      (is (not (aborting? job)))))

  (testing "it short circuits correctly"
    (let [[result job] (run-with-short-circuiting (create-job) 0 [successful-inc failing-fn successful-inc])]
      (is (= 1 result))
      (is (aborting? job))
      (is (= ["Cannot succeed!"] (-> job :job-errors)))))

  (testing "it short circuits correctly"
    (let [[result job] (run-with-short-circuiting (create-job) 0 [successful-inc exception-fn successful-inc])]
      (is (= nil result))
      (is (aborting? job))
      (is (= "foobar" (-> job :job-errors first))))))
