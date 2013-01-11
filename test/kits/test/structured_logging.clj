(ns kits.test.structured-logging
  (:require [clojure.tools.logging :as log])
  (:use [clojure.test :only [deftest is]]
        [clojure.tools.logging :only [error info warn]]
        [conjure.core :only [mocking
                             verify-first-call-args-for-indices]]
        [kits.structured-logging :only [in-context
                                        log-time
                                        logging-exceptions]]))

(deftest all-public-vars-have-docstrings
  (is (= [] (remove (comp :doc meta) (vals (ns-publics 'kits.structured-logging))))))

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
            "{\"level\":\"warn\",\"function\":\"kits.test.structured-logging/warn-calling-fn\",\"namespace\":\"kits.test.structured-logging\",\"data\":{\"c\":3,\"d\":4}}")))

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
            "{\"context\":{\"transaction/id\":\"txn123\",\"request/id\":\"req123\"},\"level\":\"error\",\"function\":\"kits.test.structured-logging/error-calling-fn\",\"namespace\":\"kits.test.structured-logging\",\"data\":{\"c\":3,\"d\":4}}")))

(deftest test-logging-exceptions
  (mocking [log/log*]
           (try
             (logging-exceptions (throw (Exception. "BOOM")))
             (is (= false "If you see this, there is a test failure. An Exception should have been thrown."))
             (catch Exception _))
           (verify-first-call-args-for-indices
            log/log*
            [1 2]
            :error
            nil))) ;; wanted to test the string logged here, but with
                   ;; the stacktrace in it, it is very hard to use equality on it


(deftest test-log-time
  (mocking [log/log*] ;; this is here to suppress console output
           (is (= 2 (log-time :test {}
                              (inc 1))))))