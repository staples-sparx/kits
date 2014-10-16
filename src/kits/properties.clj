(ns kits.properties
  "Simple way to load Java property files from classpath"
  (:use
   kits.foundation)
  (:require
   [kits.io :as io])
  (:import
   java.util.Properties))

(defn load-from-classpath
  "Load a property file from the classpath and return the equivalent
  Clojure map."
  [^String file-name]
  (let [props (Properties.)
        stream (-> (.getClass file-name)
                   (.getResourceAsStream (str "/" file-name)))]
    (if stream
      (try
        (.load props stream)
        (into {}
              (map (fn [key]
                     [key (.getProperty props key)])
                   (.stringPropertyNames props)))
        (finally
         (.close stream)))
      {})))

(defn map->props [m]
  (let [props (Properties.)]
    (doseq [[k v] m]
      (.setProperty props (name k) (str v)))
    props))
