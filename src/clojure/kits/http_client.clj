(ns ^{:doc "Bare bone, performance oriented abstraction layer on top or URLConnection"}
  kits.http-client
  (:use
    kits.foundation)
  (:refer-clojure :exclude [get])
  (:require
    [kits.string :as str]
    [kits.json :as json]
    [kits.io :as io])
  (:import
    java.io.ByteArrayOutputStream
    java.io.ByteArrayInputStream
    java.io.File
    java.io.InputStream
    java.io.OutputStream
    java.io.OutputStreamWriter
    java.io.Writer
    java.io.BufferedWriter
    java.net.HttpURLConnection
    java.net.URL
    java.net.URLConnection
    java.net.URLEncoder
    java.util.zip.GZIPInputStream
    java.util.zip.GZIPOutputStream))

(set! *warn-on-reflection* true)

(def typical-request-size 10240)

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

(def typical-request-size 10240)

(defn ensure-content-type [^String content-type]
  (or
    content-type
    "text/plain; charset=utf-8"))

(defn get-input-stream ^InputStream [^HttpURLConnection conn transparent-decode?]
  (let [is (.getInputStream conn)]
    (if transparent-decode?
      (if (= "gzip" (str/downcase (.getHeaderField conn "Content-Encoding")))
        (GZIPInputStream. is)
        is)
      is)))

(defn read-raw-resp
  [^HttpURLConnection conn transparent-decode?]
  ;; Note: no need to close the connection. From HttpURLConnection
  ;; javadoc : Each HttpURLConnection instance is used to make a
  ;; single request but the underlying network connection to the HTTP
  ;; server may be transparently shared by other instances. Calling
  ;; the close() methods on the InputStream or OutputStream of an
  ;; HttpURLConnection after a request may free network resources
  ;; associated with this instance but has no effect on any shared
  ;; persistent connection.
  (let [status (.getResponseCode ^HttpURLConnection conn)]
    (if (< 199 status 300)
      (with-open [in (get-input-stream conn transparent-decode?)
                  out (ByteArrayOutputStream. 1024)]
        (io/copy in out)
        {:status status
         :msg (.getResponseMessage ^HttpURLConnection conn)
         :headers {"Content-Type" (.getHeaderField conn "Content-Type")
                   "Content-Encoding" (.getHeaderField conn "Content-Encoding")}
         :out out})
      (if-let [error-stream (.getErrorStream conn)]
        (with-open [in error-stream
                    out (ByteArrayOutputStream. 1024)]
          (try
            (io/copy in out)
            {:status status
             :msg (.getResponseMessage ^HttpURLConnection conn)
             :out out}
            (catch Exception e
              {:status status
               :msg (.getResponseMessage ^HttpURLConnection conn)
               :out nil})))
        (try
          (with-open [in (.getInputStream conn)
                      out (ByteArrayOutputStream. 1024)]
            (io/copy in out)
            {:status status
             :msg (.getResponseMessage ^HttpURLConnection conn)
             :body (.toString out)})
          (catch Exception e
            {:status status
             :msg (.getResponseMessage ^HttpURLConnection conn)
             :out nil}))))))

(defn read-binary-resp [^HttpURLConnection conn]
  (let [raw (read-raw-resp conn false)]
    (assoc raw
      :body (when-let [out ^ByteArrayOutputStream (:out raw)]
              (ByteArrayInputStream.
                (.toByteArray out))))))

(defn read-str-resp [^HttpURLConnection conn]
  (let [raw (read-raw-resp conn true)]
    (assoc raw
      :body (when-let [out ^ByteArrayOutputStream (:out raw)]
              (.toString ^ByteArrayInputStream out)))))

(defn post-binary [url ^InputStream in timeout-ms & [content-type]]
  ;; Binary post is left untouched. In particular it is /not/
  ;; gunzipped, whatever the content encoding is
  (let [ct (ensure-content-type content-type)
        url (URL. (url-for url nil))
        conn (doto ^HttpURLConnection (.openConnection url)
                   (.setDoOutput true)
                   (.setRequestMethod "POST")
                   (.setRequestProperty
                    "Content-Type" ct)
                   (.setConnectTimeout timeout-ms)
                   (.setReadTimeout timeout-ms))]
    (with-open [out (.getOutputStream conn)]
      (io/copy in out 10240)
      (.flush out)
      (.close out)
      (read-binary-resp conn))))

(defn get
  "Returns the response of a plain http get in the form of :status, :msg, and stringyfied :body"
  [url params timeout-ms]
  (let [url (URL. (url-for url params))
        conn (doto ^HttpURLConnection (.openConnection url)
               (.setRequestMethod "GET")
               (.setConnectTimeout timeout-ms)
               (.setReadTimeout timeout-ms))]
    (read-str-resp conn)))

(defn post [url ^String data timeout-ms & [content-type gzipped-request? gzipped-response?]]
  (let [actual-content-type (ensure-content-type content-type)
        url (URL. (url-for url nil))
        conn (doto ^HttpURLConnection (.openConnection url)
                   (.setDoOutput true)
                   (.setRequestMethod "POST")
                   (.setRequestProperty
                    "Content-Type" actual-content-type)
                   (.setConnectTimeout timeout-ms)
                   (.setReadTimeout timeout-ms))
        stream-encoder (if gzipped-request?
                         #(GZIPOutputStream. %)
                         identity)
        _ (if gzipped-request?
            (doto ^HttpURLConnection conn
              (.setRequestProperty "Content-Encoding" "gzip")))
        _ (if gzipped-response?
            (doto ^HttpURLConnection conn
              (.setRequestProperty "Accept-Encoding" "gzip")))]
    (with-open [out (.getOutputStream conn)
                writer ^Writer (-> out
                                 stream-encoder
                                 OutputStreamWriter.)]
      (.write writer data)
      (.flush writer)
      (.close writer)
      (.close out)
      (read-str-resp conn))))

(defn post-json [url data timeout-ms & {:keys [gzipped-request? gzipped-response? gzipped?]
                                        :or {gzipped-request? false
                                             gzipped-response? false}}]
  (post
    url
    (json/encode-str data)
    timeout-ms
    "application/json"
    ;; Maintain backwards compatibility
    (or gzipped? gzipped-request?)
    (or gzipped? gzipped-response?)))
