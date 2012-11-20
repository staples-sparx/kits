(ns kits.test.structured-logging
  (:use clojure.test
        conjure.core
        kits.structured-logging)
  (:require [clojure.tools.logging :as log]
            [kits.core :as c]))

(deftype HasNoDefaultSerializer []
  Object
  (toString [_] "<HasNoDefaultSerializer>"))

(defn info-calling-fn []
  (info {:a 1 :b 2 :c (HasNoDefaultSerializer.)} :tags [:my-special-error]))

(defn warn-calling-fn []
  (warn {:c 3 :d 4}))

(defn error-calling-fn []
  (error {:c 3 :d 4}))

(deftest test-info-log-level
  (mocking [log/log*]
           (info-calling-fn)
           (verify-first-call-args-for-indices
            log/log*
            [1 2 3]
            :info
            nil
            "{\"tags\":[\"my-special-error\"],\"level\":\"info\",\"function\":\"kits.test.structured-logging/info-calling-fn\",\"namespace\":\"kits.test.structured-logging\",\"data\":{\"a\":1,\"b\":2,\"c\":\"<HasNoDefaultSerializer>\"}}")))

(deftest test-warn-log-level
  (mocking [log/log*]
           (warn-calling-fn)
           (verify-first-call-args-for-indices
            log/log*
            [1 2 3]
            :warn
            nil
            "{\"tags\":[],\"level\":\"warn\",\"function\":\"kits.test.structured-logging/warn-calling-fn\",\"namespace\":\"kits.test.structured-logging\",\"data\":{\"c\":3,\"d\":4}}")))

(deftest test-error-log-level---and-contexts
  (mocking [log/log*]
           (in-context {:request/id "req123"}
                       (in-context {:transaction/id "txn123"}
                                   (error-calling-fn)))
           (verify-first-call-args-for-indices
            log/log*
            [1 2 3]
            :error
            nil
            "{\"context\":{\"transaction/id\":\"txn123\",\"request/id\":\"req123\"},\"tags\":[],\"level\":\"error\",\"function\":\"kits.test.structured-logging/error-calling-fn\",\"namespace\":\"kits.test.structured-logging\",\"data\":{\"c\":3,\"d\":4}}")))


