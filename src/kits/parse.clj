(ns kits.parse
  "Thin Clojure wrappers for Java parse functions.")

(set! *warn-on-reflection* true)


(defmacro with-nil-exception
  "Evaluate body, returning nil in the event of an exception."
  [& body]
  `(try
     ~@body
     (catch Exception e# nil)))


(defn str->byte "Parses a string into a byte."
  [str-val]
  (Byte/parseByte str-val))

(defn str->short "Parses a string into a short."
  [str-val]
  (Short/parseShort str-val))

(defn str->int "Parses a string into a integer."
  [str-val]
  (Integer/parseInt str-val))

(defn str->long "Parses a string into a long."
  [str-val]
  (Long/parseLong str-val))

(defn str->float "Parses a string into a floating."
  [str-val]
  (Float/parseFloat str-val))

(defn str->double "Parses a string into a double."
  [str-val]
  (Double/parseDouble str-val))


(defn str->byte-safe "Parses a string into a byte, returning nil in the event of an exception."
  [str-val]
  (with-nil-exception (Byte/parseByte str-val)) )

(defn str->short-safe "Parses a string into a short, returning nil in the event of an exception."
  [str-val]
  (with-nil-exception (Short/parseShort str-val)) )

(defn str->int-safe "Parses a string into a integer, returning nil in the event of an exception."
  [str-val]
  (with-nil-exception (Integer/parseInt str-val)) )

(defn str->long-safe "Parses a string into a long, returning nil in the event of an exception."
  [str-val]
  (with-nil-exception (Long/parseLong str-val)) )

(defn str->float-safe "Parses a string into a floating, returning nil in the event of an exception."
  [str-val]
  (with-nil-exception (Float/parseFloat str-val)) )

(defn str->double-safe "Parses a string into a double, returning nil in the event of an exception."
  [str-val]
  (with-nil-exception (Double/parseDouble str-val)) )

