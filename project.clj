(defproject com.runa/kits "1.5.16"
  :description "Runa base libraries"
  :plugins [[jonase/eastwood "0.0.2"]
            [lein-cloverage "1.0.2"]
            [lein-kibit "0.0.7"]
            [lein-swank "1.4.4"]
            [s3-wagon-private "1.1.2"]]
  :repositories {"releases" {:url "s3p://runa-maven/releases/"
                             :creds :gpg}}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [cheshire "5.0.1"]
                 [org.clojars.runa/clj-utils "1.2.8"]
                 [org.clojure/java.jdbc "0.2.3"]
                 [org.clojure/tools.logging "0.2.4"]
                 [clojure-csv/clojure-csv "2.0.0-alpha1"]]
  :warn-on-reflection true
  :profiles {:dev {:dependencies [[org.clojars.runa/conjure "2.0.0"]
                                  [lein-multi "1.1.0"]
                                  [slamhound "1.3.3"]]}
             :1.3.0 {:dependencies [[org.clojure/clojure "1.3.0"]]}
             :1.4.0 {:dependencies [[org.clojure/clojure "1.4.0"]]}
             :1.5.1 {:dependencies [[org.clojure/clojure "1.5.1"]]}})

(cemerick.pomegranate.aether/register-wagon-factory!
 "s3p" #(eval '(org.springframework.aws.maven.PrivateS3Wagon.)))
