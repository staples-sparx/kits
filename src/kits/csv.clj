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
   :strict false})

(defn read-csv
  ([csv-rdr]
     (read-csv csv-rdr *parse-opts*))
  ([csv-rdr opts]
     (let [merged-opts (merge *parse-opts* opts)
           {:keys [skip-header delimiter end-of-line
                   quote-char strict skip-blank-lines]} merged-opts]
       (->> (csv/parse-csv csv-rdr
                           :delimiter delimiter
                           :end-of-line end-of-line
                           :quote-char quote-char
                           :strict strict)
            (#(if skip-header (rest %) %))
            (filter #(not= [""] %))))))

(defn- apply-field-opts-on-row [csv-row field-reader-opts]
  (let [exclude-columns (:exclude-columns field-reader-opts)
        add-column (fn [m i]
                     (let [col (get csv-row i)
                           field (get field-reader-opts i)]
                       (if (and field
                                (not (contains? exclude-columns i)))
                         (assoc m (:label field) ((:reader field) col))
                         m)))]
    (reduce add-column {} (range (count csv-row)))))

(defn- apply-field-opts [m csv-row {:keys [key-fn val-fn pred-fn]
                                    :or {val-fn identity
                                         key-fn identity
                                         pred-fn (constantly true)}
                                    :as field-reader-opts}]
  (let [row (apply-field-opts-on-row csv-row field-reader-opts)]
    (if (pred-fn row)
      (assoc m (key-fn row) (val-fn row))
      m)))

(defn csv-rows->map [csv-rows field-reader-opts]
  (reduce #(apply-field-opts %1 %2 field-reader-opts) {} csv-rows))

(defn csv-rows->coll [csv-rows field-reader-opts]
  (map (comp val first) (apply-field-opts csv-rows field-reader-opts)))
