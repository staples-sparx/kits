(ns ^{:doc "Logging with Clojure data"}
  kits.structured-logging
  (:require [clojure.tools.logging :as log]
            [cheshire.custom :as cc]))


(defn unmangle
  "Given the name of a class that implements a Clojure function, returns the function's name in Clojure."
  [class-name]
  (.replace
    (clojure.string/replace class-name #"^(.+)\$(.+)$" "$1/$2") \_ \-))

(defmacro current-function-name
  "Returns a string, the name of the current Clojure function."
  []
  `(-> (Throwable.)
       .getStackTrace
       first
       .getClassName
       unmangle))

;; logs assorted Objects sanely: good for logging functions or
;; assorted objects
(cc/add-encoder Object cc/encode-str)

(defn structured-log [log-level tags the-ns calling-fn-name log-map]
  (log/logp log-level (cc/encode {:tags (vec tags)
                                  :level log-level
                                  :function calling-fn-name
                                  :namespace (str the-ns)
                                  :data log-map})))


;; Use these for logging. The above are public because I can't
;; get these funky macros to work with them private

(defmacro info [log-map & {:keys [tags]}]
  `(structured-log :info ~tags ~*ns* (current-function-name) ~log-map))

(defmacro warn [log-map & {:keys [tags]}]
  `(structured-log :warn ~tags ~*ns* (current-function-name) ~log-map))

(defmacro error [log-map & {:keys [tags]}]
  `(structured-log :error ~tags ~*ns* (current-function-name) ~log-map))
