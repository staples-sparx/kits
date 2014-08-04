(ns ^{:doc "Functions for generating hex digests"}
  kits.hexdigest
  (:import [java.security MessageDigest]))

(defn hexdigest [^String hash-algo ^String s]
  (let [^MessageDigest hash (MessageDigest/getInstance hash-algo)]
    (.update hash (.getBytes s))
    (->> (.digest hash)
         (map #(format "%02x" (bit-and % 0xff)))
         (apply str))))

(defn md5 [s]
  (hexdigest "MD5" s))

(defn sha1 [s]
  (hexdigest "SHA-1" s))

(defn sha256 [s]
  (hexdigest "SHA-256" s))
