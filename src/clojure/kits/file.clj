(ns kits.file
  "File system related functions."
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:import (java.io File)))

(set! *warn-on-reflection* true)

(defn filter-line-seq
  "Filter one file into another file one line at a time.
  Only needs to keep one line in memory at a time."
  [pred input output]
  (with-open [in (io/reader input)
              out (io/writer output)]
    (binding [*out* out]
      (doseq [line (line-seq in)]
        (when (pred line)
          (println line))))))

(defn map-line-seq
  "Map one file into another file one line at a time.
   Only needs to keep one line in memory at a time."
  [f input output]
  (with-open [in (io/reader input)
              out (io/writer output)]
    (binding [*out* out]
      (doseq [line (line-seq in)]
        (println (f line))))))

(def file-separator File/separator)

(defn files-in-dir [dir]
  (->> dir
       io/file
       file-seq
       (filterv #(.isFile ^File %))))

(defn path
  "Concatenate pieces of a path into a full path.  Adds file separators when
   missing and squashing multiple file separators into one."
  [& file-path-pieces]
  (str/replace (str/join file-separator file-path-pieces)
               (re-pattern (str file-separator "+"))
               file-separator))

(defmacro with-temp-file
  "Bind var to a temp File instance and invoke body, and delete the
  file on return."
  [var & body]
  `(let [~var (io/file (str "/tmp/" (uuid)))]
     (try
       ~@body
       (finally
         (when (.exists ~var)
           (.delete ~var))))))

(defn mkdir-p
  "Create directory and parent directories if any"
  [^String path]
  (let [f ^File (File. path)]
    (.mkdirs f)))
