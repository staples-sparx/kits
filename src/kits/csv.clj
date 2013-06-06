(ns kits.csv
  "wrapper around clojure-csv library that turn csv in to column-name,
   column-value key value pair that can be configured via :key-fn, :val-fn
   and :reader for each field"
  (:require [clojure-csv.core :as csv])
  (:import java.util.HashMap))

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
           {:keys [skip-header delimiter end-of-line
                   quote-char strict]} merged-opts
           rows (csv/parse-csv csv-rdr
                               :delimiter delimiter
                               :end-of-line end-of-line
                               :quote-char quote-char
                               :strict strict)]
       (if skip-header
         (rest rows)
         rows))))

(defn- apply-field-opts-on-row [csv-row field-reader-opts]
  (let [exclude-columns (:exclude-columns field-reader-opts)]
    (reduce merge (for [i (range (count csv-row))
                        :when (or (nil? exclude-columns )
                                  (not (contains? exclude-columns i)))
                        :let [ifield (get field-reader-opts i)
                              irow (get csv-row i)]]
                    (assoc {} (:label ifield) ((:reader ifield) irow))))))

(defn- hash-map->mutable [m]
  (doto (HashMap.)
    (#(doseq [[k v] m]
        (.put % k v)))))

(defn- apply-field-opts [csv-rows {:keys [key-fn val-fn mutable?]
                                   :as field-reader-opts}]
  (map (fn [row]
         (let [row' (apply-field-opts-on-row row field-reader-opts)]
           {(key-fn row') ((if mutable? hash-map->mutable identity)
                           (val-fn row'))}))
       csv-rows))

(defn csv-rows->map [csv-rows {:keys [mutable?] :as field-reader-opts}]
  (let [rows (reduce merge (apply-field-opts csv-rows field-reader-opts))]
    (if mutable?
      (hash-map->mutable rows)
      rows)))

(defn csv-rows->coll [csv-rows {:keys [mutable?] :as field-reader-opts}]
  (let [rows (->> (apply-field-opts csv-rows field-reader-opts)
                  (sort-by (comp key first)) ; key :foo in [{:foo {...}} ...]
                  (map (comp val first)))] ; val {...} in [{:foo {...}} ...]
    (if mutable?
      (into-array rows)
      rows)))
