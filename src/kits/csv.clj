(ns kits.csv
  "wrapper around clojure-csv library that turn csv in to column-name, column-value key value
   pair that can be configured via :key-fn, :val-fn and :reader for each field "
  (:require
   [clojure-csv.core :as csv]))


(def ^:dynamic *parse-opts*
  {:skip-header false
   :delimiter \,
   :end-of-line nil
   :quote-char \"
   :strict false})

(defn read-csv
  ([csv-rdr]
     (read-csv csv-rdr *parse-opts*))
  ([csv-rdr opts]
     (let [merged-opts (merge *parse-opts* opts)
           {:keys [skip-header delimiter end-of-line quote-char strict]} merged-opts
           rows (csv/parse-csv csv-rdr
                               :delimiter delimiter
                               :end-of-line end-of-line
                               :quote-char quote-char
                               :strict strict)]
       (if skip-header
         (rest rows)
         rows))))

(defn- csv-row->map [csv-row field-reader-opts]
  (let [exclude-columns (:exclude-columns field-reader-opts)]
    (reduce merge (for [i (range (count csv-row))
                        :when (or (nil? exclude-columns )
                                  (not (contains? exclude-columns i)))
                        :let [ifield (get field-reader-opts i)
                              irow (get csv-row i)]]
                    (assoc {} (:label ifield) ((:reader ifield) irow))))))

(defn csv-rows->map [csv-rows field-reader-opts]
  (reduce merge
          (map (fn [row]
                 (let [amap (csv-row->map row field-reader-opts)]
                   (assoc {} ((:key-fn field-reader-opts) amap)
                          ((:val-fn field-reader-opts) amap)))) csv-rows)))


