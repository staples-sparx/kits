(ns kits.http-client-test
  (:use
    kits.foundation
    clojure.test)
  (:require
   [kits.http-client :as http]
   [kits.json :as json]))

(set! *warn-on-reflection* false)

(deftest http-get
  (let [resp (http/get "http://staples-sparx.com/" {} 60000)
        body (:body resp)]
    (is (= {:status 200
            :headers {"Content-Type" "text/html; charset=utf-8"
                      "Content-Encoding" nil}
            :msg "OK"}
          (dissoc resp :body :out)))
    (is (:out resp))
    (is (string? body))
    (is (< 0 (.indexOf body "small team")) body)))

(deftest http-post
  (let [resp (http/post "http://httpbin.org/post" "test=1" 60000)
        body (:body resp)]
    (is (= {:status 200
            :headers {"Content-Type" "application/json" "Content-Encoding" nil}
            :msg "OK"}
           (-> resp
               (dissoc :body :out)
               )))
    (is (:out resp))
    (is (string? body))
    (is (< 0 (.indexOf body "test=1")) body)))


(deftest http-post-gzip
  (let [resp (http/post "http://httpbin.org/post" "test=1" 60000 "text/plain" true)
        body (:body resp)]
    (is (= {:status 200
            :headers {"Content-Type" "application/json" "Content-Encoding" nil}
            :msg "OK"}
           (-> resp
               (dissoc :body :out)
               )))
    (is (:out resp))
    (is (string? body))
    (is (< 0 (.indexOf body "data:application/octet-stream;base64,H4sIAAAAAAAAACtJLS6xNQQAM5HnYQYAAAA=")) body)))
