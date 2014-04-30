(ns kits.match-test
  (:use clojure.test
        kits.match))

(set! *warn-on-reflection* false)


(deftest ^:unit test-matches
  (is (matches? "www.google.com/docs" ["*google.com*" "*yahoo.com*"]))
  (is (matches? "yahoo.com" ["*www.google.com*" "*yahoo*"]))
  (is (matches? "google.com?search=tem" ["*google.com?search*" "*yahoo.com*"]))
  (is (not (matches? "not.com" ["www.google.com" "www.yahoo.com"])))
  (is (not (matches? nil ["www.google.com" "www.yahoo.com"])))
  (is (not-matches? "not.com" ["www.google.com" "www.yahoo.com"]))
  (is (not-matches? "" ["www.google.com" "www.yahoo.com"])))

(deftest ^:unit test-matches-with-wildcard
  (is (matches? "yahoo.com" ["www.google.com" "yahoo*"]))
  (is (not (matches? "oo" ["www.google.com" "yahoo*"])))
  (is (not (matches? "blahyahoo" ["www.google.com" "yahoo*"])))
  (is (not (matches? "yahoo.com" ["www.google.com" "*yahoo"])))
  (is (matches? "blahyahoo" ["www.google.com" "*yahoo"]))
  (is (matches? "yahooblahblahcom" ["www.google.com" "yahoo*com"]))
  (is (not (matches? "yahooblahblahcomblah" ["www.google.com" "yahoo*com"]))))

(deftest ^:unit test-matches-with-collection
  (is (matches? ["lemontree"] ["lemon*" "pres-solitaire"]))
  (is (matches? ["whatever" "president"] ["lemon" "pres*"]))
  (is (matches? ["whatever" "president"] ["lemon" "pres*"]))
  (is (not-matches? ["whatever" "noway"] ["lemon" "pres"]))
  (is (not-matches? ["lemontree" "noway"] ["*lemon" "pres"])))
