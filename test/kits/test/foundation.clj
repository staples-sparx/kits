(ns kits.test.foundation
  (:use
   clojure.test)
  (:require
   [clojure.string :as str]
   [kits.foundation :as f]))


;; WARNING: Use of this namespace is deprecated.  Please stop depending on it whereever you can.

(deftest raise
  (is (thrown? RuntimeException (f/raise "test exception")))
  )

(deftest parse-numbers

  (is (nil? (f/parse-int "foo")))
  (is (nil? (f/parse-long nil)))
  (is (= 1 (f/parse-int "1")))
  (is (= 1.0 (f/parse-double "1.0")))
  (is (= 1.0 (f/parse-float "1.0")))

  )

(deftest str->boolean
  (is (true? (f/str->boolean "true")))
  (is (true? (f/str->boolean "foo")))
  (is (false? (f/str->boolean "false")))
  (is (false? (f/str->boolean "")))
  (is (false? (f/str->boolean nil)))
  )

(deftest with-progress-reporting
  (is (= (str/join "\n"
                   [".........      10 rows"
                    ".........      20 rows"
                    ".........      30 rows"
                    ".........      40 rows"
                    ".........      50 rows"
                    "      50 rows\n"])
         (with-out-str
           (f/with-progress-reporting {:iters-per-row 10 :num-columns 10}
             (dotimes [i 50]
               (report!))))))

  (is (= ""
         (with-out-str
           (binding [f/*print-progress* false]
             (f/with-progress-reporting {:iters-per-row 10 :num-columns 10}
               (dotimes [i 50]
                 (report!))))))))

(deftest seq-to-map
  (is (= {:foo 1 :bar 2} (f/seq-to-map [:foo 1 :bar 2])))
  (is (= {} (f/seq-to-map nil)))
  (is (= {} (f/seq-to-map [])))
  )

(deftest dotted-to-ip
  (are [expected dotted]
    (= expected (f/dotted-to-ip dotted))

    2130706433 "127.0.0.1"
    67240450 "4.2.2.2"
    )
  
  (is (thrown? RuntimeException (= (f/dotted-to-ip "127.0.0.1&abc") 2130706433)))

  )

(deftest ip-to-dotted
  (are [expected ip]
    (= expected (f/ip-to-dotted ip))

    "127.0.0.1" 2130706433
    "4.2.2.2" 67240450
    "4.2.144.224" 67277024
    "4.2.144.231" 67277031
    ))

(deftest parse-url
  (are [expected spec]
    (= expected (f/parse-url spec))

    {:scheme "file"
     :path "/foo/bar.clj"}
    "/foo/bar.clj"

    {:scheme "http"
     :host "runa.com"
     :path "/foo/bar.clj"}
    "http://runa.com/foo/bar.clj"

    {:scheme "http"
     :username "user"
     :password "passwd"
     :host "runa.com"
     :path "/foo/bar.clj"}
    "http://user:passwd@runa.com/foo/bar.clj"
     
    {:scheme "s3"
     :host "pythia-production-logs"
     :path "/foo/bar.clj"}
    "s3://pythia-production-logs/foo/bar.clj"

    {:scheme "file", :path "/"} ""

    nil nil))

(deftest with-timeout
  (is (thrown? RuntimeException
               (f/with-timeout 10
                 (reduce + 0 (range 1000000)))))

  (is (= :timeout
         (try
           (f/with-timeout 1
             (Thread/sleep 2000)
             (println "this shouldn't have been printed:"
                      (reduce + 0 (range 10000000))))
           (catch Exception ex
             :timeout))))

  (is (= 4950
         (try
           (f/with-timeout 100
             (reduce + 0 (range 100)))
           (catch RuntimeException ex
             :timeout))))
  )

(deftest rmerge
  (are [expected base override]
    (= expected (f/rmerge base override))

    {:a 1 :b 2} {:b 2} {:a 1}
    {:a {:x {:y {:z 3}}} :b 2} {:a {:x 1} :b 2} {:a {:x {:y {:z 3}}}}
    {:a {:x {:y {:z 3}}} :b 2} {:a {:x {:y {:z 1}}} :b 2} {:a {:x {:y {:z 3}}}}

    ))

(deftest base-array?

  (is (f/base-array? (byte-array 10)))

  (is (false? (f/base-array? [1 2 3])))

  (is (false? (f/base-array? nil)))

  )

(deftest value-and-elapsed-time

  (f/bind-value-and-elapsed-time [val elapsed]
      (+ 1 1)

    (is (= 2 val))

    (is (number? elapsed))

    )

  )
