(ns kits.csv
  "wrapper around opencsv library that turn csv in to column-name,
   column-value key value pair that can be configured via :key-fn, :val-fn
   and :reader for each field"
  (:require
   [clojure.java.io :as jio]
   [clojure.data.csv :as csv])
  (:import
   (com.opencsv CSVParser)))

(set! *warn-on-reflection* true)

(def ^:private default-opts
  {:escape \\,
   :ignoreLeadingWhiteSpace true,
   :ignoreQuotations false,
   :pending false,
   :quotechar \",
   :separator \,,
   :strictQuotes false
   :skip-header false
   :error-handler (fn [e coll]
                    (println :kits/read-csv :malformed-csv-line coll)
                    (clojure.stacktrace/print-stack-trace e))})

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

(defn- make-csv-parser [opts]
  (let [opt (merge default-opts opts)]
    (CSVParser. (:separator opt) (:quotechar opt) (:escape opt)
                (:strictQuotes opt) (:ignoreLeadingWhiteSpace opt)
                (:ignoreQuotations opt))))

(defn- parse-line [parser line error-handler]
  (when (seq line)
    (try
      (parser line)
      (catch Exception e
        (error-handler e line)
        nil))))

(defn read-csv [csv-file-path & [opts]]
  (let [encoding (or (:encoding opts)
                     "UTF-8")
        csv-parser (or (:line-parser opts)
                       #(.parseLine ^CSVParser (make-csv-parser opts) %))
        file-s (slurp csv-file-path :encoding encoding)
        all-lines (.split ^String file-s (or (:end-of-line opts) "\n"))
        lines (if (:skip-header opts)
                (rest all-lines)
                all-lines)]
    (loop [[h & t] lines
           parsed []]
      (if (nil? h)
        parsed
        (recur t (if-let [ph (parse-line csv-parser
                                         h
                                         (or (:error-handler opts)
                                             (:error-handler default-opts)))]
                   (conj parsed ph)
                   parsed))))))

(defn write-csv [csv-file data]
  (with-open [out-file (jio/writer csv-file)]
    (csv/write-csv out-file data)))
