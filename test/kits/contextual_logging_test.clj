(ns kits.contextual-logging-test
  (:require [org.rathore.amit.utils.logger :as amit])
  (:use clojure.test
        conjure.core
        kits.contextual-logging))

(set! *warn-on-reflection* false)

(use-fixtures :once (fn [f]
                      (init-async-logging-queue)
                      (init-thread-pool)
                      (f)))

(deftest all-public-vars-have-docstrings
  (is (= [] (remove (comp :doc meta) (vals (ns-publics 'kits.contextual-logging))))))

(deftest test-log-message-no-context
  (doseq [log-fn [log-message async-log-message]]
    (mocking [amit/log-message]
             (log-fn "The thing failed")
             (Thread/sleep 50)
             (verify-called-once-with-args amit/log-message "The thing failed"))))

(deftest test-log-message
  (doseq [log-fn [log-message async-log-message]]
    (mocking [amit/log-message]
             (in-log-context {:session-id "a1b2"}
                             (log-fn "The thing failed"))
             (Thread/sleep 50)
             (verify-called-once-with-args amit/log-message "session-id: a1b2 - The thing failed"))))

(deftest test-log-message-nested-contexts
  (doseq [log-fn [log-message async-log-message]]
    (mocking [amit/log-message]
             (in-log-context {:a "a" :b "b"}
                             (in-log-context {:c "c" :d "d"}
                                             (in-log-context {:e "e" :f "f"}
                                                             (log-fn "The thing failed"))))
             (Thread/sleep 50)
             (verify-called-once-with-args amit/log-message "a: a, b: b, c: c, d: d, f: f, e: e - The thing failed"))))

(deftest test-log-exception-no-context
  (doseq [log-fn [log-exception async-log-exception]]
    (mocking [amit/log-exception]
             (log-fn (Exception. "The thing failed"))
             (Thread/sleep 50)
             (verify-call-times-for amit/log-exception 1))))

(deftest test-log-exception
  (doseq [log-fn [log-exception async-log-exception]]
    (mocking [amit/log-exception]
             (in-log-context {:merchant-id "m1"}
                             (log-fn (Exception. "The thing failed") "extra message"))
             (Thread/sleep 50)
             (verify-first-call-args-for-indices amit/log-exception [1] "merchant-id: m1 - extra message"))))
