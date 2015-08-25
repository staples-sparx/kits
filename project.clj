(defproject staples-sparx/kits "1.22.0"
  :description "Staples SparX core libraries."
  :local-repo ".m2"
  :min-lein-version "2.0.0"
  :license {:name "MIT License"
            :url "http://mit-license.org/"}
  :url "https://github.com/staples-sparx/kits"
  :plugins [[jonase/eastwood "0.0.2"]
            [lein-kibit "0.0.7"]]
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/core.async "0.1.346.0-17112a-alpha"]
                 [cheshire "5.3.1"]
                 [clj-http "2.0.0"]
                 [clout "1.1.0"]
                 [org.hdrhistogram/HdrHistogram "1.2.1"]
                 [org.clojure/math.numeric-tower "0.0.2"]
                 [org.clojars.runa/clj-utils "1.3.1"]
                 [org.clojars.runa/runa.tools.logging "0.2.7"]
                 [clojure-csv/clojure-csv "2.0.0-alpha1"]
                 [org.eclipse.jetty/jetty-server "9.2.3.v20140905"]]
  :clean-targets  [ :target-path ]
  :global-vars {*warn-on-reflection* false}
  :profiles {:dev {:dependencies [[slamhound "1.3.3"]
                                  [cider/cider-nrepl "0.8.2"]]}
             :1.4 {:dependencies [[org.clojure/clojure "1.4.0"]]}
             :1.5 {:dependencies [[org.clojure/clojure "1.5.1"]]}
             :1.6 {:dependencies [[org.clojure/clojure "1.6.0"]]}
             :1.7 {:dependencies [[org.clojure/clojure "1.7.0"]]}}
  :source-paths      ["src/clojure"]
  :java-source-paths ["src/java"]
  :aliases {"run-tests" ["with-profile" "1.4.0:1.5.0" "test"]
            "slamhound" ["run" "-m" "slam.hound"]}
  :jvm-opts ["-server"
             "-XX:+UseConcMarkSweepGC"
             "-XX:+UseParNewGC"
             "-XX:+CMSParallelRemarkEnabled"
             "-XX:MaxGCPauseMillis=5"
             "-XX:+UseStringCache"
             "-XX:+OptimizeStringConcat"
             "-XX:+UseCompressedOops"
             "-XX:+AggressiveOpts"
             ;; "-XX:+UnlockCommercialFeatures"
             ;; "-XX:+FlightRecorder"
             ])
