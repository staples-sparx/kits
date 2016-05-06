(ns kits.csv
  "wrapper around opencsv library that turn csv in to column-name,
   column-value key value pair that can be configured via :key-fn, :val-fn
   and :reader for each field"
  (:require
   [clojure.java.io :as jio]
   [clojure.data.csv :as csv]
   [clojure.stacktrace :as strace]
   [clojure.string :as s])
  (:import
   (java.io InputStreamReader BufferedReader FileInputStream PushbackReader)
   (com.opencsv CSVParser)))

(set! *warn-on-reflection* true)

(defn is-line-sep-char? [line-sep c]
  (some (set line-sep) (str c)))

(defn read-lines [file-path & [opts]]
  "Intended to be used to read in large files."
  (let [lines-to-read (atom (or (:max-lines opts) 50,000) )
        fis (FileInputStream. ^String file-path)
        isr (InputStreamReader. ^java.io.FileInputStream fis
                                (or ^String (:encoding opts) "UTF-8"))
        br (PushbackReader. isr)
        line-sep (or (:end-of-line opts) "\n")
        end-of-line (StringBuilder.)
        accumulate-lines (fn [sb acc]
                           (let [s (.. ^StringBuilder sb toString trim)]
                             (if (seq s)
                               (do (swap! lines-to-read dec)
                                   (conj acc s))
                               acc)))]
    (loop [c (.read br)
           [result line-sb eol] [[] (StringBuilder.) (StringBuilder.)]]
      (if (or (= c -1) (zero? @lines-to-read))
        (conj result (.toString line-sb))
        (recur (.read br)
               (let [ch (char c)
                     eol (if (is-line-sep-char? line-sep ch)
                           (.append end-of-line ch)
                           end-of-line)
                     sb (->  ^StringBuilder line-sb
                             (.append ch))]
                 (if (= (.toString eol) line-sep)
                   [(accumulate-lines sb result)
                    (doto sb (.setLength 0))
                    (doto eol (.setLength 0))]
                   [result
                    sb
                    eol])))))))

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
                    (strace/print-stack-trace e))})

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
  (let [csv-parser (make-csv-parser opts)
        line-parser (if (:multi-line opts)
                      #(.parseLineMulti ^CSVParser csv-parser %)
                      #(.parseLine ^CSVParser csv-parser %))
        parser (or (:line-parser opts) line-parser)
        ;; file-s (-> (slurp csv-file-path :encoding encoding)
        ;;            s/trim)
        ;; all-lines (.split ^String file-s (or (:end-of-line opts) "\n"))
        all-lines (read-lines csv-file-path opts)
        lines (if (:skip-header opts)
                (rest all-lines)
                all-lines)]
    (loop [[h & t] lines
           parsed []]
      (if (nil? h)
        parsed
        (recur t (if-let [ph (parse-line parser
                                         h
                                         (or (:error-handler opts)
                                             (:error-handler default-opts)))]
                   (conj parsed ph)
                   parsed))))))

(def write-default-options
  [:separator \,
   :quote \"
   :newline :lf])

(defn write-csv [csv-file data & options]
  (with-open [out-file (jio/writer csv-file)]
    (let [opts (conj write-default-options options)]
      (csv/write-csv out-file data opts))))
