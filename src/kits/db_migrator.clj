(ns kits.db-migrator
  "SQL schema migration library"
  (:require [clojure.java.jdbc :as jdbc])
  (:import (java.sql Timestamp)))


(defn- run-and-record [db [migration-name migration-seq]]
  (println "***" migration-name "***")
  (jdbc/execute! db migration-seq)
  (jdbc/execute! db ["COMMIT"])
  (jdbc/insert! db
                :migrations
                [:name :created_at]
                [migration-name (Timestamp. (System/currentTimeMillis))]))

(defn- validate-migrations [migrations]
  (let [names (map first migrations)]
    (when-not (distinct? names)
      (throw (Exception. "There's a duplicate name in your migrations: " names)))

    (doseq [[migration-name migration-seq] migrations]
      (when-not (sequential? migration-seq)
        (throw (Exception. (str "Right side of migration '" migration-name "' was not sequential.\n Bad migration-seq: " (pr-str migration-seq))))))))

(defn migrate
  "Takes a db connection map and a sequence of migrations, like such:
   [[\"Add permalink field\" [\"ALTER TABLE foo ADD INDEX ...\"]]
    [...]]
   It executes only those schema migration statements which have not already
   been executed, and marks their names in a table called `migrations`.
   Creates the `migrations` table if it does not exist.`"
  [db migrations]
  (validate-migrations migrations)
  (jdbc/execute! db ["CREATE TABLE IF NOT EXISTS migrations (
                      `id` int(11) NOT NULL AUTO_INCREMENT,
                      `name` varchar(250) NOT NULL,
                      `created_at` datetime NOT NULL,
                      PRIMARY KEY (`id`))"])

  (println "Running migrations...")
  (let [run-migration-set (->> (jdbc/query db ["SELECT name FROM migrations"])
                               (map :name)
                               set)]
    (doseq [[migration-name _ :as migration] migrations
            :when (not (run-migration-set migration-name))]
      (run-and-record db migration)))
  (println "Migrations complete."))
