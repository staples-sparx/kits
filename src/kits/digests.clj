(ns kits.digests
  "Functions for computing digests via one-way cryptographic hash functions"
  (:use
    kits.foundation)
  (:require
    [kits.io :as io])
  (:import
    java.io.FileInputStream
    java.io.InputStream
    java.security.MessageDigest))

(defn digest-str ^MessageDigest [^String algo ^String s]
  (when s
    (let [d (MessageDigest/getInstance algo)]
      (.update d (.getBytes s))
      d)))

(defn digest-stream ^MessageDigest [^String algo ^InputStream in ^long chunk-size]
  (when in
    (let [d (MessageDigest/getInstance algo)
          buf (make-array Byte/TYPE chunk-size)]
      (loop []
        (let [size (.read in buf)]
          (when (pos? size)
            (.update d ^bytes buf 0 size)
            (recur))))
      d)))

(defn digest-file ^MessageDigest [^String algo ^String file-path]
  (when (io/exists? file-path)
    (digest-stream
      algo
      (FileInputStream. file-path)
      4096)))

(defn ->hex [^MessageDigest d]
  (when d
    (->> (.digest d)
      (map #(format "%02x" (bit-and % 0xff)))
      (apply str))))

(defn md5 [s]
  (->hex
    (digest-str "MD5" s)))

(defn sha1 [s]
  (->hex
    (digest-str "SHA-1" s)))

(defn sha256 [s]
  (->hex
    (digest-str "SHA-256" s)))

(defn md5-for-path [^String file-path]
  (->hex
    (digest-file "MD5" file-path)))
