(ns kits.xml
  "To simplify working with clojure.xml"
  (:require [clojure.xml :as xml])
  (:import java.io.ByteArrayInputStream))


(defn parse [xml-string]
  (xml/parse (ByteArrayInputStream. (.getBytes (.trim xml-string)))))

(defn content-of-node-with-tag [nodes & [tag & more-tags]]
  (if more-tags
    (apply content-of-node-with-tag (content-of-node-with-tag nodes tag) more-tags)
    (:content (first (filter #(= tag (:tag %)) nodes)))))