(ns ^{:doc "Bare bone, performance oriented abstraction layer on top or URLConnection"}
    kits.http-client
  (:refer-clojure :exclude [get])
  (:require [cheshire.core :as json])
  (:import java.io.ByteArrayOutputStream
           java.io.File
           java.io.InputStream
           java.io.OutputStream
           java.io.OutputStreamWriter
           java.io.Writer
           java.io.BufferedWriter
           java.net.HttpURLConnection
           java.net.URL
           java.net.URLConnection
           java.net.URLEncoder))

(set! *warn-on-reflection* false)

(defn url-encode [value]
  (URLEncoder/encode (str value)))

(defn encode-params [params]
  (->> params
       (map (fn [[k v]]
              (str k "=" (url-encode (json/encode v)))))
       (interpose "&")
       (apply str)))

(defn copy
  ([in out]
   (copy in out 10240))
  ([^InputStream in ^OutputStream out chunk-size]
   (let [buf (make-array Byte/TYPE chunk-size)]
     (loop []
       (let [size (.read in buf)]
         (when (pos? size)
           (.write out ^bytes buf 0 size)
           (recur)))))))

(defn url-for [url params]
  (if (empty? params)
    url
    (str url "?" (encode-params params))))

(defn read-response [^HttpURLConnection conn]
  (let [status (.getResponseCode conn)]
    (if (= 200 status)
      (with-open [in (.getInputStream conn)
                  out (ByteArrayOutputStream. 1024)]
        (copy in out)
        {:status status
         :msg    (.getResponseMessage conn)
         :body   (.toString out)})
      (with-open [in (.getErrorStream conn)
                  out (ByteArrayOutputStream. 1024)]
        (copy in out)
        {:status status
         :msg    (.getResponseMessage conn)
         :body   (.toString out)}))))

(defn get [url params timeout]
  (let [url (URL. (url-for url params))
        conn (doto ^HttpURLConnection (.openConnection url)
               (.setRequestMethod "GET")
               (.setConnectTimeout timeout)
               (.setReadTimeout timeout))]
    (read-response conn)))

(defn post-json [url params timeout]
  (let [data ^String (json/encode params)
        url (URL. url)
        conn (doto ^HttpURLConnection (.openConnection url)
               (.setDoOutput true)
               (.setRequestMethod "POST")
               (.setRequestProperty
                 "Content-Type",
                 "application/json")
               (.setConnectTimeout timeout)
               (.setReadTimeout timeout))]
    (with-open [out (.getOutputStream conn)
                outWriter (OutputStreamWriter. out)
                writer (BufferedWriter. outWriter)]
      (.write writer data)
      (.flush writer)
      (.close writer)
      (.close out)
      (read-response conn))))
