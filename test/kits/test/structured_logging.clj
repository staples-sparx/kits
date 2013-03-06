(ns kits.test.structured-logging
  (:require [clojure.tools.logging :as log]
            [kits.timestamp :as ts])
  (:use clojure.test
        conjure.core
        kits.structured-logging))

(deftest all-public-vars-have-docstrings
  (is (= [] (remove (comp :doc meta) (vals (ns-publics 'kits.structured-logging))))))

(deftype HasNoDefaultSerializer []
  Object
  (toString [_] "<HasNoDefaultSerializer>"))

(defn info-calling-fn []
  (info {:a 1 :b 2 :c (HasNoDefaultSerializer.) :tags [:my-special-error]}))

(defn warn-calling-fn []
  (warn {:c 3 :d 4}))

(defn error-calling-fn []
  (error {:c 3 :d 4 :tags [:bad-csv-row]}))

(deftest test-info-log-level
  (stubbing [log/log* nil
             ts/now 123456789]
           (info-calling-fn)
           (verify-first-call-args-for-indices
            log/log*
            [1 2 3]
            :info
            nil
            "{\"tags\":[\"my-special-error\"],\"level\":\"INFO\",\"timestamp\":\"1970-01-02 10:17:36\",\"function\":\"kits.test.structured-logging/info-calling-fn\",\"namespace\":\"kits.test.structured-logging\",\"data\":{\"a\":1,\"b\":2,\"c\":\"<HasNoDefaultSerializer>\"}}")))

(deftest test-warn-log-level
  (stubbing [log/log* nil
             ts/now 123456789]
           (warn-calling-fn)
           (verify-first-call-args-for-indices
            log/log*
            [1 2 3]
            :warn
            nil
            "{\"level\":\"WARN\",\"timestamp\":\"1970-01-02 10:17:36\",\"function\":\"kits.test.structured-logging/warn-calling-fn\",\"namespace\":\"kits.test.structured-logging\",\"data\":{\"c\":3,\"d\":4}}")))

(deftest test-error-log-level---and-contexts
  (stubbing [log/log* nil
             ts/now 123456789]
           (in-log-context (do {:request/id "req123" :tags [:import]})
                           (in-log-context {:transaction/id "txn123"}
                                           (is (= {:request/id "req123"
                                                   :transaction/id "txn123"
                                                   :tags [:import]}
                                                  (log-context)))
                                   (error-calling-fn)))
           (verify-first-call-args-for-indices
            log/log*
            [1 2 3]
            :error
            nil
            "{\"context\":{\"request/id\":\"req123\",\"transaction/id\":\"txn123\"},\"tags\":[\"import\",\"bad-csv-row\"],\"level\":\"ERROR\",\"timestamp\":\"1970-01-02 10:17:36\",\"function\":\"kits.test.structured-logging/error-calling-fn\",\"namespace\":\"kits.test.structured-logging\",\"data\":{\"c\":3,\"d\":4}}")))

(deftest test-logging-exceptions
  (stubbing [log/log* nil
             ts/now 123456789]
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
  (stubbing [log/log* nil] ;; this is here to suppress console output
           (is (= 2 (log-time :test {}
                              (inc 1))))))