(ns kits.volatile-byte-array-test
  (:use clojure.test)
  (:import (kits VolatileByteArray)))

(deftest ^:unit byte-arrays-crud
  (let [ba (VolatileByteArray. 1024)
        test-data (byte-array [(byte 42) (byte 27) (byte 3)])]
    (testing "put and read"
      (.put ba test-data)
      (is (= (aget test-data 0)
             (aget (.get ba) 0)))
      (is (= (aget test-data 1)
             (aget (.get ba) 1)))
      (is (= (aget test-data 2)
             (aget (.get ba) 2))))
    (testing "clear"
      (.put ba test-data) ;; Make sure that both arrays have data
      (.clear ba)
      (is (= (byte 0)
             (aget (.get ba) 0)))
      (is (= (byte 0)
             (aget (.get ba) 1))))
    (testing "multiple puts"
      (.put ba test-data)
      (.put ba test-data)
      (.put ba test-data)
      (.put ba test-data))))
