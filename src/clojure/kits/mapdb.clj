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
  (def csv (kits.csv/read-csv "/home/kasim/Downloads/title-similarity-2016-06-30-0000.csv"))
  (def cmap (kits.csv/csv-rows->map csv field-reader-opts))
  (def ks (keys cmap))
  (def ts-db (make-mmapfile-db "title-similarity.db"))
  (load-data->mapdb (second ts-db) cmap)
  (def ts-db-m (second ts-db))
  (use 'criterium.core)
  (bench (.get ts-db-m (rand-nth ks)))
  (.commit (first ts-db))
  (.close (first ts-db))
  )

(comment
 "Context: JVM memory set to 12GB min and max. The file size is 1.3G, which is the biggest
  model we have so far. It has about 1.5 millian keys and it is loaded into memory mapped
  file. JVM flags used are: -server -XX:+UseG1GC.
  The result of (bench (.get ts-db-m (rand-nth ks))). Note that we are paying the price
  of (rand-nth ks), which I benched after this."
 "****************************************************************************************"
  "Evaluation count : 240 in 60 samples of 4 calls.
             Execution time mean : 267.249250 ms
    Execution time std-deviation : 24.264187 ms
   Execution time lower quantile : 226.476043 ms ( 2.5%)
   Execution time upper quantile : 312.330598 ms (97.5%)
                   Overhead used : 16.773196 ns

Found 1 outliers in 60 samples (1.6667 %)
        low-severe       1 (1.6667 %)
 Variance from outliers : 65.2626 % Variance is severely inflated by outliers"


  "The result of (bench (rand-nth ks)): "
  "****************************************************************************************"
  "Evaluation count : 300 in 60 samples of 5 calls.
             Execution time mean : 259.192548 ms
    Execution time std-deviation : 23.245407 ms
   Execution time lower quantile : 226.009207 ms ( 2.5%)
   Execution time upper quantile : 298.428629 ms (97.5%)
                   Overhead used : 16.773196 ns

Found 2 outliers in 60 samples (3.3333 %)
        low-severe       1 (1.6667 %)
        low-mild         1 (1.6667 %)
 Variance from outliers : 65.2277 % Variance is severely inflated by outliers"

  "THE VERDICT: "
  "****************************************************************************************"
  "At 97.5% the net cost we pay for looking up a key from memory mapped file is:
   (- 312.330598 298.428629) => 13.901969000000008ms ~ 14ms
   So, I'd say it is practical for us to use in Trillian where we cap service time at
   30ms. (Note: The SLA is 100ms)"

  )
