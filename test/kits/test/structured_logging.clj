(ns kits.structured-logging
  (:use clojure.test
        conjure.core
        kits.structured-logging)
  (:require [clojure.tools.logging :as log]
            [kits.core :as c]))

(defn our-function []
  (info {:a 1 :b 2} :tags [:my-special-error]))

(defn our-function-2 []
  (warn {:c 3 :d 4}))

;(when-1-3+
  (deftest test-logging
    (mocking [log/log*]
      (our-function)
      (verify-first-call-args-for-indices
        log/log*
        [1 2 3]
        :info
        nil
        "{:tags [:my-special-error], :level :info, :function \"kits.structured-logging-test/our-function\", :namespace \"kits.structured-logging-test\", :data {:a 1, :b 2}}")))

  (deftest test-logging
    (mocking [log/log*]
      (our-function-2)
      (verify-first-call-args-for-indices
        log/log*
        [1 2 3]
        :warn
        nil
        "{:tags [], :level :warn, :function \"kits.structured-logging-test/our-function-2\", :namespace \"kits.structured-logging-test\", :data {:c 3, :d 4}}")))  ;)