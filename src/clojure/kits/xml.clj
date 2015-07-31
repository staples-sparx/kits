(ns kits.xml
  "To simplify working with clojure.xml"
  (:require [clojure.xml :as xml])
  (:import (java.io StringBufferInputStream)))

(set! *warn-on-reflection* true)


(defn parse [xml-string]
  (xml/parse (StringBufferInputStream. xml-string)))

(defn content-of-node-with-tag [nodes & [tag & more-tags]]
  (if more-tags
    (apply content-of-node-with-tag (content-of-node-with-tag nodes tag) more-tags)
    (:content (first (filter #(= tag (:tag %)) nodes)))))
