(ns kits.http-client-test
  (:use
    kits.foundation
    clojure.test)
  (:require
    [kits.http-client :as http]))

(set! *warn-on-reflection* false)

(deftest http-get
  (let [resp (http/get "http://staples-sparx.com/" {} 60000)
        body (:body resp)]
    (is (= {:status 200
            :msg "OK"} (dissoc resp :body)))
    (is (string? body))
    (is (< 0 (.indexOf body "small team")) body)))
