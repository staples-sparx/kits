(ns kits.digests-test
  (:use
    clojure.test
    kits.foundation)
  (:require
    [kits.digests :as digests]))

(deftest md5
  (are [expected s]
    (= expected (digests/md5 s))

    nil nil
    "d41d8cd98f00b204e9800998ecf8427e" ""
    "8b1a9953c4611296a827abf8c47804d7" "Hello"
    "7d793037a0760186574b0282f2f435e7" "world"))

(deftest sha1
  (are [expected s]
    (= expected (digests/sha1 s))

    nil nil
    "da39a3ee5e6b4b0d3255bfef95601890afd80709" ""
    "f7ff9e8b7bb2e09b70935a5d785e0cc5d9d0abf0" "Hello"
    "7c211433f02071597741e6ff5a8ea34789abbf43" "world"))

(deftest sha256
  (are [expected s]
    (= expected (digests/sha256 s))

    nil nil
    "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855" ""
    "185f8db32271fe25f561a6fc938b2e264306ec304eda518007d1764826381969" "Hello"
    "486ea46224d1bb4fb680f34f7c9ad96a8f24ec88be73ea8e5a6c65260e9cb8a7" "world"))

;; TODO - ph7 - Better cleanup strategy for the files

(deftest md5-for-path
  (let [hello "/tmp/hello"
        world "/tmp/world"
        empty "/tmp/empty"]

    (spit hello "Hello")
    (spit world "World")
    (spit empty "")

    (are [expected path]
      (= expected (digests/md5-for-path path))

      "8b1a9953c4611296a827abf8c47804d7" hello
      "f5a7924e621e84c9280a9a27e1bcb7f6" world
      "d41d8cd98f00b204e9800998ecf8427e" empty

      nil nil
      nil "/Does/Not/Exists.csv"

      )))
