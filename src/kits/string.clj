(ns kits.string
  (:require [clojure.string :as str])
  (:import (java.net URLDecoder)))

(defn split [s delim]
  (cond
    (nil? s)        nil
    (string? delim) (str/split s (re-pattern delim))
    :else           (str/split s delim)))

(defn blank? [s]
  (empty? s))

(defn lowercase [s]
  (str/lower-case s))

(defn strip-whitespace [s]
  (str/replace s #"[\s\t\r]" ""))

(defn stringify-map-values [the-map]
  (let [stringify-entry (fn [[k v]] {k (str v)})]
    (apply merge (map stringify-entry the-map))))

(def escape-regex-except-*
  (memoize (fn 
         ;"Escapes special Regex symbols except *."
             [^String s]
             (.. s
                 (replace "\\" "\\\\")
                 (replace "("  "\\(")
                 (replace ")"  "\\)")
                 (replace "."  "\\.")
                 (replace "["  "\\[")
                 (replace "]"  "\\]")
                 (replace "^"  "\\^")
                 (replace "$"  "\\$")
                 (replace "|"  "\\|")
                 (replace "?"  "\\?")
                 (replace "*"  ".*") ;; Only WildChar * will work
                 (replace "+"  "\\+")))))

(defn decode-string [s]
  (when s
    (let [ds (.replace (URLDecoder/decode (str s) "UTF-8") " " "_")]
      (.replaceAll (re-matcher #"[\W]" ds) "_"))))

(defn urldecode [s]
  (when s
    (URLDecoder/decode s "UTF-8")))

(defn mapify [s sep]
  (when s
    (let [kv (split s sep)]
      (hash-map (first kv) (or (second kv) "")))))

(defn domain-name [host-name]
  (let [hns (split host-name #"\.")]
    (if (= 1 (count hns))
      (first hns)
      (nth hns (- (count hns) 2)))))

(defn parse-url [url-str]
  (when-not (empty? url-str)
    (try
      (let [url (java.net.URL. url-str)]
	{:host (.getHost url) :query (.getQuery url)})
      (catch java.net.MalformedURLException e
	(println "Trying to get QUERY string from Mal Formed URL :: " url-str)))))

(defn query-string [url-str]
  (when-let [url-m (parse-url url-str)]
    (:query url-m)))

(defn parse-query-param
  [q]
  (if q
    (reduce (fn [a s] (merge a (mapify s "="))) {} (split q "&"))
    {}))

(defn build-query-params
  [p]
  (org.apache.commons.lang.StringUtils/chop (str "?" (apply str (map (fn [a] (str (first a) "=" (last a) "&")) p)))))

(defn query-url [url param]
  (when-let [query (query-string url)]
    ((parse-query-param query) param)))

(defn as-str
  ([] "")
  ([x] (if (instance? clojure.lang.Named x)
         (name x)
         (str x))))

(defn extract-number [s]
  (when s
    (if (number? s)
      s
      (read-string (last (re-seq #"[\-]?[0-9]+[.,]?[0-9]*" s))))))

(defn hyphenize [s]
  (when s
    (str/replace s #"_" "-")))

(defn decamelize [s]
  (when s
    (letfn [(hump? [a b] (and a b
                           (Character/isLowerCase ^Character a)
                           (Character/isUpperCase ^Character b)))
            (smooth-hump [result ch]
              (if (hump? (last result) ch)
                (str result \- ch)
                (str result ch)))]
      (str/lower-case (reduce smooth-hump "" s)))))

(defn all-whitespace? [s]
  (if s
    (boolean (re-matches #"^\s*$" s))
    false))

(defn nil-or-empty-string? [s]
  (or (nil? s) (= "" s)))

(defn nil-str [x]
  (when x (str x)))

(defn empty-str
  "returns nil if x is empty or nil, otherwise x as a string"
  [x]
  (if (or (string? x)
          (sequential? x))
    (when-not (empty? x)
      (str x))
    (nil-str x)))

(defn git-sha? [s]
  (if s
    (boolean (re-matches #"([a-f]|\d){40}" s))
    false))

(defn repeat-str [n x]
  (apply str (repeat n x)))

(defn uuid? [s]
  (if s
    (boolean (re-matches #"[a-f0-9]{8}-[a-f0-9]{4}-[a-f0-9]{4}-[a-f0-9]{4}-[a-f0-9]{12}" s))
    false))

(defn uuid+timestamp? [s]
  (if s
    (boolean (re-matches #"[a-f0-9]{8}-[a-f0-9]{4}-[a-f0-9]{4}-[a-f0-9]{4}-[a-f0-9]{12}-\d{13}" s))
    false))

(defn remove-dashes [guid]
  (str/replace guid #"-" ""))

;; Keyword functions

(defn underscores->hyphens [k]
  (when k
    (-> k name (.replace "_" "-") keyword)))

(defn hyphens->underscores [k]
  (when k
    (-> k name (.replace "-" "_") keyword)))
