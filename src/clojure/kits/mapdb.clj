(ns kits.mapdb
  "a thin wrapper around Java mapdb"
  (:import (org.mapdb DB DBMaker)
           (java.util.concurrent ConcurrentMap)))

(set! *warn-on-reflection* true)

(defn- make-mmapfile-db [^String file]
  (let [db (.. (DBMaker/fileDB file)
               fileMmapEnable
               fileMmapPreclearDisable
               cleanerHackEnable
               make)]
    [db (.. db (hashMap "map") make)]))

(defn load-mmap-db [db k v & opts]
  (doto ^ConcurrentMap db
    (.put k v)))

(defn load-data->mapdb [db data & opts]
  (doseq [[k v] data]
    (load-mmap-db db k v opts)))

(comment
  (def db (make-mmapfile-db "kasim2.db"))
  (.. (first db) getStore fileLoad)
  (load-data->mapdb (second db) {"test" "me"
                                 "over" "again"
                                 "map" {1 3}
                                 "set" #{1 3}
                                 "coll" [1 2 3 4]})
  (.get (second db) "test")
  (.get (second db) "over")
  (.get (second db) "map")
  (.get (second db) "set")
  (.get (second db) "coll")
  (.commit (first db))
  (.close (first db)))

(comment
  (def field-reader-opts {:key-fn :src-sku
                          0 {:label :src-sku :reader identity}
                          1 {:label :target-sku :reader identity}
                          2 {:label :score :reader identity}})
  (def csv (kits.csv/read-csv "~/Downloads/title-similarity-2016-06-30-0000.csv"))
  (def cmap (kits.csv/csv-rows->map csv field-reader-opts))
  (def ts-db (make-mmapfile-db "title-similarity.db"))
  (def load-data->mapdb (second db) cmap)
  (.commit (first db))
  (.close (first db))
  )




