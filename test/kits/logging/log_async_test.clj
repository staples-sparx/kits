(ns kits.logging.log-async-test
  (:require 
    [clojure.test :refer :all]
    [clojure.java.shell :refer [sh]]
    [clojure.string :as string]
    [kits.logging.log-async :as log]))

(deftest start-stop-logging
  (try 
    (let [log-cfg {:root "/tmp"
                   :filename-prefix "foo-log"
                   :default-context "foo-context"
                   :thread-count 2
                   :thread-prefix "log-"
                   :rotate-every-minute 1
                   :max-msg 10000
                   :max-unflushed 3
                   :max-elapsed-unflushed-ms 200
                   :queue-timeout-ms 100}
          log-q-atom (atom nil)
          pool (log/start-thread-pool! log-q-atom log-cfg)]
      (is (some? @log-q-atom))
      (dotimes [n 100]
        (log/info @log-q-atom {:message "Hello world!"
                               :n n})
        (log/warn @log-q-atom {:message "Beware world!"
                               :n n})
        (log/exception @log-q-atom (ex-info "Oh crap!" {:stuff "some data"}) {:message2 "Some error happened!"}))
      (log/stop-thread-pool! log-q-atom pool 1000)
      (let [wc (:out (sh "/bin/sh" :in "wc -l /tmp/foo-log-*.log\n"))
            lines (-> wc (string/split #"\n") (last) (string/trim) (string/split #"\s") (first))]
        (is (= (str 300) lines))))
    (finally
      (sh "/bin/sh" :in "rm -rf /tmp/foo-log*\n"))))
