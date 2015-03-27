(ns ^{:doc "Shallow wrapping around our best know JSON parsing library of the time"}
  kits.json
  (:require
    [cheshire.core :as core])
  (:import
    java.io.BufferedReader
    java.io.BufferedWriter))

(defn ^String encode-str
  "Returns a JSON-encoding String for the given object."
  [v]
  (core/generate-string v ))

(defn encode
  "Write a JSON-encoded object to the given writer"
  [v ^BufferedWriter out]
  (core/generate-stream out))

(defn decode-str
  "Returns the Clojure object corresponding to the given JSON-encoded string."
   [^String json]
  (core/parse-string json))

(defn decode
  "Returns the Clojure object corresponding the first JSON-encoded read on the given reader."
  [^BufferedReader in]
  (core/parse-stream in))

(defn resilient-encode-str
  ([dict] (resilient-encode-str dict (fn [ex] 
                                       (binding [*out* *err*]
                                         (println (str "Failed to encode JSON from: '" dict "'"))
                                         (.printStackTrace ex))
                                       "{}"))) ; return an empty json datum
  ([dict handle-error]
   (try (encode-str dict)
        (catch Exception e 
          (handle-error e)))))
