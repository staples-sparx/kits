(defproject staples-sparx/kits "2.0.3"
  :description "SparX's core utilities."
  :local-repo ".m2"
  :min-lein-version "2.0.0"
  :license {:name "MIT License"
            :url "http://mit-license.org/"}
  :url "https://github.com/staples-sparx/kits"
  :plugins [[jonase/eastwood "0.0.2"]
            [lein-kibit "0.0.7"]
            [s3-wagon-private "1.1.2"]]
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [cheshire "5.3.1"]
                 [clout "1.1.0"]
                 [org.hdrhistogram/HdrHistogram "1.2.1"]
                 [org.clojure/math.numeric-tower "0.0.2"]
                 [org.clojure/java.jdbc "0.3.6"]
                 [clojure-csv/clojure-csv "2.0.1"]
                 [org.eclipse.jetty/jetty-server "9.2.3.v20140905"]]
  :clean-targets  [ :target-path ]
  :global-vars {*warn-on-reflection* false}
  :profiles {:dev {:dependencies [[slamhound "1.3.3"]]}
             :1.5.0 {:dependencies [[org.clojure/clojure "1.5.1"]]}}
  :aliases {"run-tests" ["with-profile" "1.4.0:1.5.0" "test"]
            "slamhound" ["run" "-m" "slam.hound"]}
  :deploy-repositories [["clojars" {:url "https://clojars.org/repo/"
                                    :signing {:gpg-key "76FD68DC"}}]
                        ["s3-releases" {:url "s3p://runa-maven/releases"
                                        :username [:env/archiva_username]
                                        :passphrase [:env/archiva_passphrase]}]])
