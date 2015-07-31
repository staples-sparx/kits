(ns ^{:doc "Fns to expand on the use of the Clout routing library."}
  kits.clout
  (:require [clojure.string :as str]
            [clout.core :as clout]))

(defn- strip-trailing-slash [uri]
  (str/replace uri #"(?<=.)/$" ""))

(defn find-matching-handler [routes uri]
  (some (fn [[route handler]]
          (when-let [match (clout/route-matches
                            route {:uri (strip-trailing-slash uri)})]
            {:handler handler :params match}))
        routes))
