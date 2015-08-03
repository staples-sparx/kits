(defproject staples-sparx/kits "1.21.0"
  :description "Staples SparX core libraries."
  :local-repo "../Furtive/.m2"
  :min-lein-version "2.0.0"
  :license {:name "MIT License"
            :url "http://mit-license.org/"}
  :url "https://github.com/staples-sparx/kits"
  :plugins [[jonase/eastwood "0.0.2"]
            [lein-kibit "0.0.7"]]
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [cheshire "5.3.1"]
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
             :1.4.0 {:dependencies [[org.clojure/clojure "1.4.0"]]}
             :1.5.0 {:dependencies [[org.clojure/clojure "1.5.1"]]}}
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
             ]
  :deploy-repositories [["clojars" {:url "https://clojars.org/repo/"
                                    :signing {:gpg-key "76FD68DC" }}]
                        ["s3-releases" {:url "s3p://runa-maven/releases"
                                        :username [:env/archiva_username]
                                        :passphrase [:env/archiva_passphrase]}]])
