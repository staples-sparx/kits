(ns kits.contextual-logging-test
  (:use
    clojure.test
    kits.contextual-logging)
  (:require
    [org.rathore.amit.utils.logger :as amit]))

(use-fixtures :once (fn [f]
                      (init-async-logging-queue)
                      (init-thread-pool)
                      (f)))

(deftest all-public-vars-have-docstrings
  (is (= []
        (remove
          (comp :doc meta)
          (vals (ns-publics 'kits.contextual-logging))))))

(deftest test-log-message-no-context
  (let [logs (atom nil)]
    (with-redefs [amit/log-message (fn [msg] (swap! logs str msg))]
      (doseq [log-fn [log-message async-log-message]]
        (reset! logs nil)
          (log-fn "The thing failed")
          (Thread/sleep 50)
          (is (= "The thing failed" @logs))))))

(deftest test-log-message
  (let [logs (atom nil)]
    (with-redefs [amit/log-message (fn [msg] (swap! logs str msg))]
      (doseq [log-fn [log-message async-log-message]]
        (reset! logs nil)
        (in-log-context {:session-id "a1b2"}
          (log-fn "The thing failed"))
        (Thread/sleep 50)
        (is (= "session-id: a1b2 - The thing failed" @logs))))))

;; TODO: Need to take care of this
#_(deftest test-log-message-nested-contexts
  (let [logs (atom nil)]
    (with-redefs [amit/log-message (fn [msg] (swap! logs str msg))]
      (doseq [log-fn [log-message async-log-message]]
        (reset! logs nil)
        (in-log-context {:a "a" :b "b"}
          (in-log-context {:c "c" :d "d"}
            (in-log-context {:e "e" :f "f"}
              (log-fn "The thing failed"))))
        (Thread/sleep 50)
        (is (= "a: a, b: b, c: c, d: d, e: e, f: f - The thing failed" @logs))))))

(deftest test-log-exception-no-context
  (let [logs (atom nil)]
    (with-redefs [amit/log-exception (fn [e] (swap! logs str (.getMessage e)))]
      (doseq [log-fn [log-exception async-log-exception]]
        (reset! logs nil)
        (log-fn (Exception. "The thing failed"))
        (Thread/sleep 50)
        (is (= "The thing failed" @logs))))))

(deftest test-log-exception
  (let [logs (atom nil)]
    (with-redefs [amit/log-exception (fn [e msg] (swap! logs str (.getMessage e) msg))]
      (doseq [log-fn [log-exception async-log-exception]]
        (reset! logs nil)
        (in-log-context {:merchant-id "m1"}
          (log-fn (Exception. "The thing failed") "extra message"))
        (Thread/sleep 50)
        (is (= "The thing failedmerchant-id: m1 - extra message" @logs))))))
