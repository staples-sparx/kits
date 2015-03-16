(ns ^{:doc "Bare bone, performance oriented abstraction layer on top or URLConnection"}
    kits.http-client
  (:refer-clojure :exclude [get])
  (:require
    [kits.json :as json]
    [kits.io :as io])
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
              (str k "=" (url-encode (json/encode-str v)))))
       (interpose "&")
       (apply str)))

(defn url-for [url params]
  (if (empty? params)
    url
    (str url "?" (encode-params params))))

(defn read-response [^HttpURLConnection conn]
  (let [status (.getResponseCode conn)]
    (if (= 200 status)
      (with-open [in (.getInputStream conn)
                  out (ByteArrayOutputStream. 1024)]
        (io/copy in out)
        {:status status
         :msg    (.getResponseMessage conn)
         :body   (.toString out)})
      (with-open [in (.getErrorStream conn)
                  out (ByteArrayOutputStream. 1024)]
        (io/copy in out)
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
  (let [data ^String (json/encode-str params)
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
