(ns kits.file
  (:require [clojure.string :as str]))


(defn path [& file-path-pieces]
  (str/replace (str/join "/" file-path-pieces) #"/+" "/"))
