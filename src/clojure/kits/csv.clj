(ns kits.csv
  "wrapper around clojure-csv library that turn csv in to column-name,
   column-value key value pair that can be configured via :key-fn, :val-fn
   and :reader for each field"
  (:require [clojure-csv.core :as csv]))

(set! *warn-on-reflection* true)

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
                   quote-char strict]} merged-opts]
       (->> (csv/parse-csv csv-rdr
                           :delimiter delimiter
                           :end-of-line end-of-line
                           :quote-char quote-char
                           :strict strict)
            (#(if skip-header (rest %) %))
            (filter #(not= [""] %))))))

(defn- csv-row->value [csv-row {:keys [val-fn exclude-columns]
                              :or {val-fn identity}
                              :as field-reader-opts}]
  (let [add-column (fn [m i]
                     (let [col (get csv-row i)
                           field (get field-reader-opts i)]
                       (if (and field
                                (not (contains? exclude-columns i)))
                         (assoc m (:label field) ((:reader field) col))
                         m)))]
    (->> csv-row
         count
         range
         (reduce add-column nil)
         val-fn)))

(defn csv-rows->coll [csv-rows {:keys [pred-fn]
                                :or {pred-fn (constantly true)}
                                :as field-reader-opts}]
  (->> csv-rows
       (map #(csv-row->value % field-reader-opts))
       (filter pred-fn)))

(defn csv-rows->map [csv-rows {:keys [key-fn]
                               :or {key-fn identity}
                               :as field-reader-opts}]
  (reduce #(assoc %1 (key-fn %2) %2)
          nil
          (csv-rows->coll csv-rows field-reader-opts)))
