(ns kits.parse
  "Thin Clojure wrappers for Java parse functions.")

; AWTAWT TODO:  add examples for wrapper macro to catch exceptions and return nil
; (with-nil-exceptions
;   (str->byte "xyz"))
; -> nil

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

