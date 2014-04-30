(ns kits.runtime-test
  (:use clojure.test)
  (:require [kits.runtime :as r]))

(set! *warn-on-reflection* false)

(deftest smoke-test
  (is (not (nil? (r/process-info))))
  (is (not (nil? (r/jvm-info))))
  (is (not (nil? (r/host))))
  (is (not (nil? (r/process-id))))
  (is (not (nil? (r/thread-id))))
  (is (not (nil? (r/hostname))))
  (is (not (nil? (r/ip-address))))
  (is (>= (r/load-average) 0))
  (is (> (r/processor-count) 0))
  (is (>= 2 (r/cpu-usage) 0))
  (is (not (nil? (r/os-info))))
  (is (not (nil? (r/os-memory))))
  (is (not (nil? (r/jvm-overall-memory))))
  (is (> (r/jvm-start-time) 0))
  (is (> (r/uptime) 0))
  (is (not (nil? (r/disk-usage "/"))))
  )
