(ns ^{:doc "Logging with Clojure data"}
  kits.structured-logging
  (:require [clojure.tools.logging :as log]))


(defn unmangle
  "Given the name of a class that implements a Clojure function, returns the function's name in Clojure."
  [class-name]
  (.replace
    (clojure.string/replace class-name #"^(.+)\$(.+)$" "$1/$2") \_ \-))

(defmacro current-function-name
  "Returns a string, the name of the current Clojure function."
  []
  `(-> (Throwable.) .getStackTrace first .getClassName unmangle))

(defn- structured-log [log-level tags the-ns calling-fn-name log-map]
  (log/logp log-level (pr-str {:tags (vec tags)
                               :level log-level
                               :function calling-fn-name
                               :namespace (str the-ns)
                               :data log-map})))

(defmacro info [log-map & {:keys [tags]}]
  `(#'groupon.structured-logging/structured-log :info ~tags ~*ns* (current-function-name) ~log-map))

(defmacro warn [log-map & {:keys [tags]}]
  `(#'groupon.structured-logging/structured-log :warn ~tags ~*ns* (current-function-name) ~log-map))

(defmacro error [log-map & {:keys [tags]}]
  `(#'groupon.structured-logging/structured-log :error ~tags ~*ns* (current-function-name) ~log-map))
