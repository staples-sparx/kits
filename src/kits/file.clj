(ns kits.file
  "File system related functions."
  (:require [clojure.string :as str])
  (:import (java.io File)))


(def file-separator File/separator)

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
