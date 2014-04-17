(ns kits.csv
  "wrapper around clojure-csv library that turn csv in to column-name,
   column-value key value pair that can be configured via :key-fn, :val-fn
   and :reader for each field"
  (:require [clojure-csv.core :as csv]))

(def ^:dynamic *parse-opts*
  {:skip-header false
   :delimiter \,
   :end-of-line nil
   :quote-char \"
   :skip-blank-lines false
   :strict false})

(defn read-csv
  ([csv-rdr]
     (read-csv csv-rdr *parse-opts*))
  ([csv-rdr opts]
     (let [merged-opts (merge *parse-opts* opts)
           {:keys [skip-header delimiter end-of-line
                   quote-char strict skip-blank-lines]} merged-opts
           rows' (csv/parse-csv csv-rdr
                                :delimiter delimiter
                                :end-of-line end-of-line
                                :quote-char quote-char
                                :strict strict)
           rows (if skip-blank-lines
                  (remove #(= [""] %) rows')
                  rows')]
       (if skip-header
         (rest rows)
         rows))))

(defn- apply-field-opts-on-row [csv-row field-reader-opts]
  (let [exclude-columns (:exclude-columns field-reader-opts)]
    (reduce merge (for [i (range (count csv-row))
                        :let [ifield (get field-reader-opts i)
                              irow (get csv-row i)]
                        :when (and ifield
                                   (or (nil? exclude-columns)
                                       (not (contains? exclude-columns i))))]
                    (assoc {} (:label ifield) ((:reader ifield) irow))))))

(defn- apply-field-opts [csv-rows {:keys [key-fn val-fn pred-fn]
                                   :or {val-fn identity
                                        key-fn identity
                                        pred-fn (constantly true)}
                                   :as field-reader-opts}]
  (remove nil? (map (fn [row]
                      (let [row' (apply-field-opts-on-row row field-reader-opts)]
                        (if (pred-fn row') {(key-fn row') (val-fn row')} nil)))
                    csv-rows)))

(defn csv-rows->map [csv-rows field-reader-opts]
  (reduce merge (apply-field-opts csv-rows field-reader-opts)))

(defn csv-rows->coll [csv-rows field-reader-opts]
  (map (comp val first) (apply-field-opts csv-rows field-reader-opts)))
