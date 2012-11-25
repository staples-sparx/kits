(def common-deps '[[cheshire "4.0.3"]
                   [clj-webdriver "0.6.0-alpha11"]
                   [gui-diff "0.3.8"]
                   [org.clojure/java.jdbc "0.2.3"]
                   [org.clojure/tools.logging "0.2.3"]])

(defproject com.runa/kits "1.2.21"
  :description "Runa base libraries"
  :plugins [[s3-wagon-private "1.1.2"]
            [lein-swank "1.4.4"]]
  :repositories {"releases" {:url "s3p://runa-maven/releases/"}
                 "snapshots" {:url "s3p://runa-maven/snapshots/"}}
  :dependencies ~(cons '[org.clojure/clojure "1.4.0"]
                       common-deps)
  :warn-on-reflection true
  :dev-dependencies [[jonase/kibit "0.0.6"]
                     [jonase/eastwood "0.0.2"]
                     [org.clojars.runa/conjure "2.0.0"]
                     [lein-multi "1.1.0"]]
  :multi-deps {"1.2.0" [[org.clojure/clojure "1.2.0"]]
               "1.2.1" [[org.clojure/clojure "1.2.1"]]
               "1.3.0" [[org.clojure/clojure "1.3.0"]]
               "1.4.0" [[org.clojure/clojure "1.4.0"]]
               "1.5.0" [[org.clojure/clojure "1.5.0-beta1"]]
               :all ~common-deps})
