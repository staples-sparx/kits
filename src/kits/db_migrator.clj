(ns kits.db-migrator
  "SQL schema migration library"
  (:require [clojure.java.jdbc :as jdbc]
            [clojure.java.io :as io])
  (:import (java.sql Timestamp)))


(defn- run-and-record [[migration-name migration-fn]]
  (println "***" migration-name "***")
  (migration-fn)
  (jdbc/do-commands "COMMIT")
  (jdbc/insert-values "migrations"
                     [:name :created_at]
                     [migration-name (Timestamp. (System/currentTimeMillis))]))

(defn- validate-migrations [migrations]
  (let [names (map first migrations)]
    (when-not (distinct? names)
      (throw (Exception. "There's a duplicate name in your migrations: " names)))

    (doseq [[migration-name migration-fn] migrations]
      (when-not (fn? migration-fn)
        (throw (Exception. (str "Right side of migration '" migration-name "' was not a function i.e. (not (fn? x)).")))))))

(defn migrate
  "Takes a db connection map and a sequence of migrations, like such:
     [[\"Add permalink field\" (fn [] (do-commands \"ALTER TABLE foo ADD INDEX ...\" ))]
      [...]]"
  [db migrations]
  (validate-migrations migrations)
  (jdbc/with-connection db
    (jdbc/do-commands "CREATE TABLE IF NOT EXISTS migrations (
                        `id` int(11) NOT NULL AUTO_INCREMENT,
                        `name` varchar(250) NOT NULL,
                        `created_at` datetime NOT NULL,
                        PRIMARY KEY (`id`))")
    
    (jdbc/transaction
     (let [has-run? (jdbc/with-query-results run ["SELECT name FROM migrations"]
                      (set (map :name run)))]
       (println "Running migrations...")
       (doseq [[migration-name migrate-fn :as migration] migrations
               :when (not (has-run? migration-name))]
         (run-and-record migration))))
    (println "Migrations complete.")))

