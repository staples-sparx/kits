(ns kits.test.contextual-logging
  (:require [org.rathore.amit.utils.logger :as amit])
  (:use clojure.test
        conjure.core
        kits.contextual-logging))

(deftest all-public-vars-have-docstrings
  (is (= [] (remove (comp :doc meta) (vals (ns-publics 'kits.contextual-logging))))))

(deftest test-log-message-no-context
  (mocking [amit/log-message]
           (log-message "The thing failed")
           (verify-called-once-with-args amit/log-message "The thing failed")))

(deftest test-log-message
  (mocking [amit/log-message]
    (in-log-context {:session-id "a1b2"}
      (log-message "The thing failed"))
    (verify-called-once-with-args amit/log-message "session-id: a1b2 - The thing failed")))

(deftest test-log-message-nested-contexts
  (mocking [amit/log-message]
    (in-log-context {:a "a" :b "b"}
      (in-log-context {:c "c" :d "d"}
        (in-log-context {:e "e" :f "f"}
          (log-message "The thing failed"))))
    (verify-called-once-with-args amit/log-message "a: a, b: b, c: c, d: d, f: f, e: e - The thing failed")))

(deftest test-log-exception-no-context
  (mocking [amit/log-exception]
    (log-exception (Exception. "The thing failed"))
    (verify-call-times-for amit/log-exception 1)))

(deftest test-log-exception
  (mocking [amit/log-exception]
    (in-log-context {:merchant-id "m1"}
      (log-exception (Exception. "The thing failed") "extra message"))
    (verify-first-call-args-for-indices amit/log-exception [1] "merchant-id: m1 - extra message")))