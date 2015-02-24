(ns ^{:doc "IO related convenience functions"}
  kits.io
  (:use
    kits.foundation)
  (:require
   [kits.logging :as log]
   [clojure.java.io :as jio])
  (:import
   java.io.BufferedReader
   java.io.ByteArrayInputStream
   java.io.ByteArrayOutputStream
   java.io.File
   java.io.FileInputStream
   java.io.FileOutputStream
   java.io.FileWriter
   java.io.IOException
   java.io.InputStream
   java.io.InputStreamReader
   java.io.OutputStream
   java.io.OutputStreamWriter
   java.io.PrintWriter
   java.io.PushbackReader
   java.io.Writer
   java.net.HttpURLConnection
   java.net.URL
   java.util.zip.ZipEntry
   java.util.zip.ZipFile))

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

(defn stream->str ^String [^InputStream in ^long expected-size ^String encoding]
  (let [out (ByteArrayOutputStream. expected-size)]
    (jio/copy in out expected-size)
    (.toString out encoding)))

(defn copy-file [^String from ^String to]
  (with-open [in (FileInputStream. from)
              out (FileOutputStream. to)]
    (copy in out)))

(defn file ^File [path] (if (string? path) (File. ^String path) path))

(defn resolve-symlink
  [^File file]
  (.getCanonicalPath file))

(defn file-name [^File path]
  (.getName path))

(defn exists? [^String path]
  (when path
    (.exists
      (java.io.File. path))))

(defn mkdir
  "Creates a directory including any missing parents"
  [^String path]
  (.mkdirs (jio/file path)))

(defn make-parents
  "Creates all parent directories of file."
  [^String path]
  (.mkdirs (.getParentFile (File. path))))

(defn touch [^String path]
  (-> (File. path)
    (.createNewFile)))

(defn extract-zip-entry [^ZipFile zf ^ZipEntry e ^String base-dir]
  (let [name (.getName e)
        dest (str base-dir "/" e)]
    (make-parents dest)
    (if (.isDirectory e)
      (mkdir dest)
      (with-open [in (.getInputStream zf e)
                  out (FileOutputStream. dest)]
        (copy in out)))))

(defn explode-zip-file [^String zip-file-path ^String where]
  (with-open [zf (ZipFile. zip-file-path)]
    (doseq [e (enumeration-seq (.entries ^ZipFile zf))]
      (log/debug "Extracting model package file " (.getName ^ZipEntry e) " -> " where)
      (extract-zip-entry zf e where))))


;; TODO - duplicate with http ns
(defn url-get [url]
  (slurp url))

;; TODO - duplicate with http ns
(defn url-post [url params]
  (let [u (URL. url)]
    (let [c (doto (.openConnection u)
                    (.setDoOutput true))]
      (with-open [out (.getOutputStream c)
                  w (OutputStreamWriter. out)]
        (.write w ^String (apply str (interpose "&" (map (fn [[n v]] (str (name n) "=" (name v))) params))))
        (.flush w))
      (with-open [in (.getInputStream c)]
        (slurp in)))))

(defn str->stream
  "Wrap a string as an input stream"
  [^String str]
  (ByteArrayInputStream. (.getBytes str)))

(defn ls [dir]
  (-> (jio/file dir)
    (.list)))

(defn dirs [^String dir]
  (filter
    #(.isDirectory ^File %)
    (-> (File. dir) (.listFiles))))

(defn read-from-classpath
  "Load a file from the classpath and returns the associated stream"
  [^String file-name]
  (-> (.getClass file-name)
    (.getResourceAsStream (str "/" file-name))))

(defmacro with-resource-line-seq [[var-name resource-path] & body]
  `(with-open [in# (read-from-classpath ~resource-path)
               r# (java.io.InputStreamReader. in#)
               br# (java.io.BufferedReader. r#)]
     (let [lines# (line-seq br#)]
       (doseq [~var-name lines#]
         ~@body))))

(defmacro with-line-seq [[var-name resource-path] & body]
  `(let [path# ~resource-path]
     (with-open [r# (java.io.FileReader. ^String path#)
                 br# (java.io.BufferedReader. r#)]
       (let [lines# (line-seq br#)]
         (doseq [~var-name lines#]
           ~@body)))))

(defmacro with-file-writer [[var-name file-path] & body]
  `(let [path# ~file-path]
     (with-open [w# (java.io.FileWriter. ^String path#)
                 ~var-name w#]
       (binding [*out* ~var-name]
         ~@body)
       (.flush ^java.io.FileWriter w#))))

(defn resilient-close
  "Close a writer ensuring that no exception can be triggered and we do not write anything to disk."
  [^Writer writer error-callback]
  (when writer
    (try
      (.close writer)
      (catch Exception e
        (error-callback e)))))

(defn resilient-write
  "Write 'data' using 'writer', but ignore any java.io.Exception. Useful
  to avoid killing a logging loop when file system is full for
  instance."
  [^Writer writer ^String data error-callback]
  (try
    (.write writer data)
    (catch IOException e
      (error-callback e))))

(defn resilient-flush
  "Flush 'writer', but ignore any java.io.Exception. Useful to avoid
  killing a logging loop when file system is full for instance."
  [^Writer writer error-callback]
  (try
    (.flush writer)
    (catch IOException e
      (error-callback e))))

(defn clj?
  "Returns true if file is a normal file with a .clj extension."
  [^File file]
  (and (.isFile file)
       (.endsWith (.getName file) ".clj")))

(defn find-clj-files-in-dir
  [^String dir]
  ;; Use sort by absolute path to get breadth-first search.
  (map #(.getPath ^File %1)
    (sort-by #(.getAbsolutePath ^File %)
      (filter clj? (file-seq (file dir))))))

(defn path->ns [^String path]
  (-> path
    (.replaceAll "/" "\\.")
    (.replaceAll "_" "-")
    (.replaceAll ".clj$" "")))

(defn find-ns-in-dir
  "Searches dir recursively for (ns ...) declarations in Clojure
  source files; returns the symbol names of the declared namespaces."
  [^String dir]
  (let [skip (inc (count dir))]
    (map symbol
      (map path->ns
        (map #(subs %1 skip)
          (find-clj-files-in-dir dir))))))

(defn expand-file-name
  "Expand leading tilde (~) into the current user home directory, or
  convert relative paths to absolute paths."
  [^String f]
  (cond
    (.startsWith f "~") (str (System/getProperty "user.home") (subs f 1))
    (.startsWith f "/") f
    :else (str (System/getProperty "user.dir") "/" f)))

(defn read-sexpr [src]
  (with-open [in (PushbackReader. (jio/reader src))]
    (read in)))

(defn write-sexpr [dest expr]
  (with-open [out (jio/writer dest)]
    (print-dup expr out)))

(defn tmp-file
  "Creates an empty file in the default temporary-file directory,
   using the given prefix and suffix to generate its name.  Returns
   its path. File will be deleted on JVM exit"
  [^String prefix ^String suffix]
  (let [f (doto
            (File/createTempFile "temp",".txt")
            (.deleteOnExit))]
    (.getPath f)))

(defn tmp-file-with
  "Creates a file with provided content in the default temporary-file directory,
   using the given prefix and suffix to generate its name. Returns its
   path. File will be deleted on JVM exit"
  [^String prefix ^String suffix ^String content]
  (let [path (tmp-file prefix suffix)]
    (spit path content)
    path))

(defn last-modified-date [^String file]
  (-> file
      java.io.File.
      .lastModified
      java.util.Date.))
