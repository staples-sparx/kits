(defproject com.runa/kits "1.0.0"
  :description "Runa base libraries"
  :plugins [[s3-wagon-private "1.1.2"]]
  :repositories {"releases" {:url "s3p://runa-maven/releases/"}
                 "snapshots" {:url "s3p://runa-maven/snapshots/"}}
  :dependencies [[org.clojure/clojure "1.3.0"]]
  :aot [kits.foundation kits.queues kits.runtime])
