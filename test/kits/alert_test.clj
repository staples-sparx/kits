(ns ^{:doc "Unit tests for alerts"}
  kits.alert-test
  (:require [clojure.test :refer :all]
            [kits.alert :as alert]
            [kits.runtime :as runtime]))

(def ^:private alert-conf
  {:mode :log
   :alert-callback #(assert (some? %))})

(defn- with-alerts-initialized
  [test-fn]
  (alert/initialize! alert-conf)
  (test-fn)
  (alert/uninitialize!))

(use-fixtures :each with-alerts-initialized)

(deftest ^:unit test-post-alert
  (alert/alert! "The sky is falling!")
  (is true))

(deftest ^:unit test-post-exception-alert
  (let [alerted? (atom false)]
    (try
      (throw (Exception. "Ohnoes!"))
      (catch Exception e
        (alert/alert! e)
        (reset! alerted? true)))
    (is @alerted?)))

(deftest ^:unit test-exception-details
  (let [exA (ex-info "Error A" {:foo :A :junk (fn [] :asdf)})
        exB (ex-info "Error B" {:bar :B} exA)
        details (alert/exception-details exB)]
    (is (= {:type "clojure.lang.ExceptionInfo"
            :message "Error B"
            :data {:bar :B}
            :cause
            {:type "clojure.lang.ExceptionInfo"
             :message "Error A"
             :data {:foo :A}}}
           (-> details
               (update-in [:cause] dissoc :stacktrace)
               (dissoc :stacktrace))))))

(deftest ^:unit test-alert-details
  (let [alert-str "Bad things!"
        alert-info (update-in {} [:details] merge (alert/alert-details))]
    (is (= {:details (runtime/process-info)}
           alert-info))))
