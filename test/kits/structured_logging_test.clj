(ns kits.structured-logging-test
  (:use
    kits.foundation)
  (:require
    [clojure.test :refer :all]
    [kits.structured-logging :refer :all]
    [kits.syslog :as syslog]
    [kits.timestamp :as ts]
    [runa.tools.logging :as log]))


;; TODO: several of these tests no longer work now that
;; we are using syslog. (# 58455260)

(def syslog-config
  {:host "127.0.0.1"
   :port 514
   :retries 10
   :max-msg-length (* 50 1024)
   :split-break-suffix " ..."
   :split-continue-prefix "... "
   :so-timeout-ms 500})

(def syslog-local-name "")

(deftest all-public-vars-have-docstrings
  (is (= [#'kits.structured-logging/syslog-channel]
         (remove (comp :doc meta) (vals (ns-publics 'kits.structured-logging))))))

(deftype HasNoDefaultSerializer []
  Object
  (toString [_] "<HasNoDefaultSerializer>"))

(def state (atom 4))

(defn error-calling-fn []
  (reset! state 88)
  (error syslog-config syslog-local-name {:c 3 :d 4 :tags [:bad-csv-row]}))

(deftest test-info-log-level
  (let [logs (atom nil)]
    (with-redefs [syslog/log (fn [config facility local-name level channel msg] (swap! logs conj [config facility local-name level msg]))
                  ts/now (constantly 123456789)]
      (info syslog-config syslog-local-name {:a 1 :b 2 :c (HasNoDefaultSerializer.) :tags [:my-special-error]})
      (is (===
            [[syslog-config
              16
              syslog-local-name
              6
              "{\"tags\":[\"my-special-error\"],\"level\":\"INFO\",\"ts-ms\":123456789,\"data\":{\"a\":1,\"b\":2,\"c\":\"<HasNoDefaultSerializer>\"}}"]]
            @logs)))))

(deftest test-warn-log-level
  (let [logs (atom nil)]
    (with-redefs [syslog/log (fn [config facility local-name level channel msg] (swap! logs conj [config facility local-name level msg]))
                  ts/now (constantly 123456789)]
      (warn syslog-config syslog-local-name {:c 3 :d 4})
      (is (===
            [[syslog-config
              16
              syslog-local-name
              4
              "{\"level\":\"WARN\",\"ts-ms\":123456789,\"data\":{\"c\":3,\"d\":4}}"
              ]] @logs)))))

(deftest test-error-log-level---and-contexts
  (let [logs (atom nil)
        state-checker (fn [] @state)]
    (with-redefs [syslog/log (fn [config facility local-name level channel msg] (swap! logs conj [config facility local-name level msg]))
                  ts/now (constantly 123456789)]
      (in-log-context (do {:request/id "req123" :state state-checker :tags [:import]})
        (in-log-context {:transaction/id "txn123"}
          (is (= {:request/id "req123"
                  :transaction/id "txn123"
                  :state state-checker
                  :tags [:import]}
                (log-context)))
          (error-calling-fn)))
      (is (===
            [[syslog-config
              16
              syslog-local-name
              3
              "{\"context\":{\"state\":88,\"request/id\":\"req123\",\"transaction/id\":\"txn123\"},\"tags\":[\"import\",\"bad-csv-row\"],\"level\":\"ERROR\",\"ts-ms\":123456789,\"data\":{\"c\":3,\"d\":4}}"
              ]] @logs)))))

;; (deftest test-exception
;;   (stubbing [syslog/log nil
;;              ts/now 123456789]
;;             (exception syslog-config syslog-local-name (Exception. "BOOM") {:message "BOOM"
;;                                                                             :time 1234})
;;            (verify-first-call-args-for-indices
;;             syslog/log
;;             [0 2 3 5]
;;             syslog-config
;;             syslog-local-name
;;             3
;;             "{\"tags\":[\"exception\"],\"level\":\"ERROR\",\"ts-ms\":123456789,\"data\":{\"stacktrace\":\"java.lang.Exception\\nBOOM\\nkits.test.structured_logging$fn__5226$fn__5227.invoke(structured_logging.clj:65)\\nclojure.core$with_redefs_fn.invoke(core.clj:6751)\\nkits.test.structured_logging$fn__5226.invoke(structured_logging.clj:63)\\nclojure.test$test_var$fn__7145.invoke(test.clj:701)\\nclojure.test$test_var.invoke(test.clj:701)\\nuser$eval83$test_var_with_selector__92.invoke(NO_SOURCE_FILE:1)\\nclojure.lang.AFn.applyToHelper(AFn.java:163)\\nclojure.lang.AFn.applyTo(AFn.java:151)\\nclojure.core$apply.invoke(core.clj:619)\\nleiningen.core.injected$compose_hooks$fn__17.doInvoke(NO_SOURCE_FILE:1)\\nclojure.lang.RestFn.applyTo(RestFn.java:137)\\nclojure.core$apply.invoke(core.clj:617)\\nleiningen.core.injected$run_hooks.invoke(NO_SOURCE_FILE:1)\\nleiningen.core.injected$prepare_for_hooks$fn__22$fn__23.doInvoke(NO_SOURCE_FILE:1)\\nclojure.lang.RestFn.applyTo(RestFn.java:137)\\nclojure.lang.AFunction$1.doInvoke(AFunction.java:29)\\nclojure.lang.RestFn.invoke(RestFn.java:408)\\nclojure.test$test_all_vars$fn__7149$fn__7156.invoke(test.clj:717)\\nclojure.test$default_fixture.invoke(test.clj:671)\\nclojure.test$test_all_vars$fn__7149.invoke(test.clj:717)\\nclojure.test$default_fixture.invoke(test.clj:671)\\nclojure.test$test_all_vars.invoke(test.clj:713)\\nclojure.test$test_ns.invoke(test.clj:736)\\nclojure.core$map$fn__4207.invoke(core.clj:2487)\\nclojure.lang.LazySeq.sval(LazySeq.java:42)\\nclojure.lang.LazySeq.seq(LazySeq.java:60)\\nclojure.lang.Cons.next(Cons.java:39)\\nclojure.lang.RT.next(RT.java:598)\\nclojure.core$next.invoke(core.clj:64)\\nclojure.core$reduce1.invoke(core.clj:896)\\nclojure.core$reduce1.invoke(core.clj:887)\\nclojure.core$merge_with.doInvoke(core.clj:2702)\\nclojure.lang.RestFn.applyTo(RestFn.java:139)\\nclojure.core$apply.invoke(core.clj:619)\\nclojure.test$run_tests.doInvoke(test.clj:751)\\nclojure.lang.RestFn.applyTo(RestFn.java:137)\\nclojure.core$apply.invoke(core.clj:617)\\nuser$eval83$fn__149.invoke(NO_SOURCE_FILE:1)\\nuser$eval83.invoke(NO_SOURCE_FILE:1)\\nclojure.lang.Compiler.eval(Compiler.java:6619)\\nclojure.lang.Compiler.eval(Compiler.java:6609)\\nclojure.lang.Compiler.eval(Compiler.java:6582)\\nclojure.core$eval.invoke(core.clj:2852)\\nclojure.main$eval_opt.invoke(main.clj:308)\\nclojure.main$initialize.invoke(main.clj:327)\\nclojure.main$null_opt.invoke(main.clj:362)\\nclojure.main$main.doInvoke(main.clj:440)\\nclojure.lang.RestFn.invoke(RestFn.java:421)\\nclojure.lang.Var.invoke(Var.java:419)\\nclojure.lang.AFn.applyToHelper(AFn.java:163)\\nclojure.lang.Var.applyTo(Var.java:532)\\nclojure.main.main(main.java:37)\"}}")))

(deftest test-logging-exceptions
  (let [logs (atom nil)]
    (with-redefs [syslog/log (fn [config facility local-name level channel msg] (swap! logs conj [config facility local-name level]))
                  ts/now (constantly 123456789)]
            (try
              (logging-exceptions syslog-config syslog-local-name (throw (Exception. "BOOM")))
              (is (= false "If you see this, there is a test failure. An Exception should have been thrown."))
              (catch Exception _))
            (is (===
                  [[syslog-config
                    16
                    syslog-local-name
                    3
                    ;; wanted to test the string logged here, but with the stacktrace in it, it is very hard to use equality on it
                    ]] @logs))
            )))

(deftest test-log-time
  (with-redefs [syslog/log (fn [& args])] ;; this is here to suppress console output
    (is (= 2 (log-time syslog-config syslog-local-name :test {}
               (inc 1))))))
