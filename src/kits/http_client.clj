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
  (URLEncoder/encode
    (str value)))

(defn encode-params [params]
  (->> params
       (map (fn [[k v]]
              (str k "=" (url-encode v))))
       (interpose "&")
       (apply str)))

(defn encode-json-params [params]
  (->> params
       (map (fn [[k v]]
              (str k "=" (url-encode (json/encode-str v)))))
       (interpose "&")
       (apply str)))

(defn url-json-for [url params]
  (if (empty? params)
    url
    (str url "?" (encode-json-params params))))

(defn url-for [url params]
  (if (empty? params)
    url
    (str url "?" (encode-params params))))

(defn read-response [^HttpURLConnection conn]
  ;; Note: no need to close the connection. From HttpURLConnection
  ;; javadoc : Each HttpURLConnection instance is used to make a
  ;; single request but the underlying network connection to the HTTP
  ;; server may be transparently shared by other instances. Calling
  ;; the close() methods on the InputStream or OutputStream of an
  ;; HttpURLConnection after a request may free network resources
  ;; associated with this instance but has no effect on any shared
  ;; persistent connection.
  (let [status (.getResponseCode ^HttpURLConnection conn)
        response-fn (fn [body]
                      {:status status
                       :msg (.getResponseMessage ^HttpURLConnection conn)
                       :body body})]
    (if (= 200 status)
      (with-open [in (.getInputStream conn)
                  out (ByteArrayOutputStream. 1024)]
        (io/copy in out)
        (response-fn (.toString out)))
      (if-let [error-stream (.getErrorStream conn)]
        (with-open [in error-stream
                    out (ByteArrayOutputStream. 1024)]
          (when in
            (io/copy in out))
          (response-fn (.toString out)))
        (response-fn nil)))))

(defn ensure-content-type [^String content-type]
  (or
    content-type
    "text/plain; charset=utf-8"))

(defn get
  "Returns the response of a plain http get in the form of :status, :msg, and stringyfied :body"
  [url params timeout-ms]
  (let [url (URL. (url-for url params))
        conn (doto ^HttpURLConnection (.openConnection url)
               (.setRequestMethod "GET")
               (.setConnectTimeout timeout-ms)
               (.setReadTimeout timeout-ms))]
    (read-response conn)))

(defn post [url data timeout-ms & [content-type]]
  (let [actual-content-type (ensure-content-type content-type)
        url (URL. (url-for url nil))
        conn (doto ^HttpURLConnection (.openConnection url)
                   (.setDoOutput true)
                   (.setRequestMethod "POST")
                   (.setRequestProperty
                    "Content-Type" actual-content-type)
                   (.setConnectTimeout timeout-ms)
                   (.setReadTimeout timeout-ms))]
    (with-open [out (.getOutputStream conn)
                writer ^Writer (OutputStreamWriter. out)]
      (.write writer data)
      (.flush writer)
      (.close writer)
      (.close out)
      (read-response conn))))

(defn post-json [url data timeout-ms]
  (post
    url
    (json/encode data)
    timeout-ms
    "application/json"))
