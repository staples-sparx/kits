(ns kits.http-client-test
  (:use
    kits.foundation
    clojure.test)
  (:require
   [kits.http-client :as http]
   [kits.json :as json])
  (:import
    java.io.InputStreamReader
    java.io.ByteArrayInputStream
    java.io.ByteArrayOutputStream
    java.util.zip.GZIPInputStream
    java.util.zip.GZIPOutputStream))

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

;; Binary post is left untouched. In particular it is /not/ gunzipped
(deftest http-post-binary
  (let [opaque-binary-data [42 17 27]
        data->bytes #(byte-array (map byte %))
        str->bytes #(into [] (.getBytes % "UTF-8"))
        out->in #(ByteArrayInputStream. (.toByteArray %))
        resp (http/post-binary
               "http://httpbin.org/post"
               (ByteArrayInputStream.
                 (data->bytes opaque-binary-data))
               60000)
        body (:body resp)]
    (is (= {:status 200
            :headers {"Content-Type" "application/json" "Content-Encoding" nil}
            :msg "OK"}
           (-> resp
               (dissoc :body :out))))
    (is (:out resp))
    (let [reply (json/decode
                  (InputStreamReader.
                    (out->in
                      (:out resp))))]
      (is (= opaque-binary-data
            (str->bytes
              (get reply "data")))))))

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
