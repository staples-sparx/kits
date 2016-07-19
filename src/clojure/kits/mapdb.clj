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

(comment
  (def db (make-mmapfile-db "kasim.db"))
  (.. (first db) getStore fileLoad)
  (load-mmap-db (second db) "test" "me")
  (load-mmap-db (second db) "over" "again")
  (load-mmap-db (second db) "map" {1 3})
  (load-mmap-db (second db) "set" #{1 3})
  (load-mmap-db (second db) "coll" [1 2 3 4])
  (.get (second db) "test")
  (.get (second db) "over")
  (.get (second db) "map")
  (.get (second db) "set")
  (.get (second db) "coll")
  (.commit (first db))
  (.close (first db))
  )




