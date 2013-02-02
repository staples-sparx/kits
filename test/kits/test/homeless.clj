(ns kits.test.homeless
  (:use clojure.test
        conjure.core
        kits.homeless))

(deftest all-kits.namespaces-have-doc-strings
  (testing "Since Kits is a set of core libraries for Runa, we want things to be well documented.
            Each namespace should have a clear Single Responsibility explained in its doc string."
    (is (= [] (->> (all-ns)
                   (filter #(.startsWith (str (ns-name %)) "kits."))
                   (remove #(.startsWith (str (ns-name %)) "kits.test"))
                   (remove (fn [ns] (:doc (meta ns)))))))))

(deftest test-raise
  (is (thrown? RuntimeException (raise "test exception"))))

(deftest test-parse-numbers
  "Tests parse-int, parse-long, parse-short, parse-float, and parse-double"
  (is (nil? (parse-int "foo")))
  (is (nil? (parse-long nil)))
  (is (= 1 (parse-int "1")))
  (is (= 1.0 (parse-double "1.0")))
  (is (= 1.0 (parse-float "1.0"))))

(deftest test-parse-number
  (are [expected str default] (= expected (parse-number str default))
    nil nil nil
    10 "10" nil
    nil "" nil
    24 "" 24
    10.0 "10.00" nil
    10.0 "10.00" 0
    10 10 0))

(deftest test-boolean?
  (are [x bool?] (= bool? (boolean? x))
    false true
    true true
    nil  false
    "ad" false
    []   false
    {}   false))

(deftest test-wrap-trapping-errors
  (is (false? ((wrap-trapping-errors pos? false) "string")))
  (is (nil? ((wrap-trapping-errors pos?) "string")))
  (is (true?  ((wrap-trapping-errors string?) "string"))))

(deftest test-ip-address-v4?
  (is (false? (ip-address-v4? "34.342.3.4")))
  (is (ip-address-v4? "192.168.0.1")))

(deftest test-str->boolean
  (is (str->boolean "true"))
  (is (false? (str->boolean "")))
  (is (false? (str->boolean "false"))))

(deftest test-base-array?
  (is (base-array? (to-array '(1 2 2 :c :d :e))))
  (is (base-array? (into-array {:a 3})))
  (is (base-array? (to-array [:a 3])))
  (is (false? (base-array? '(1 2 2 :c :d :e)))))

(deftest test-ipv4-dotted-to-integer
  (is (= (ipv4-dotted-to-integer "127.0.0.1") 2130706433)))

(deftest test-ipv4-integer-to-dotted
  (is (= "127.0.0.1" (ipv4-integer-to-dotted (ipv4-dotted-to-integer "127.0.0.1")))))

(deftest test-parse-url
  (is (= {:scheme "http", :host "www.runa.com", :path "/design"} (parse-url "http://www.runa.com/design")))
  (is (= nil (parse-url "")))
  (is (= nil (parse-url nil))))

(deftest test-url?
  (is (false? (url? "malformedhttp:// url")))
  (is (false? (url? "")))
  (is (false? (url? nil)))
  (is (url? "http://www.runa.com/")))

(deftest test-periodic-fn
  (let [msgs-logged (atom [])
        log-msg (fn [msg]
      (swap! msgs-logged conj msg))]
    (testing "creates a fn that only gets call every period times (2 in this example);
            has access to the 'call-count' as well, and the fn body is an implicit 'do'"
      (let [log-every-other (periodic-fn [msg] [call-count 2]
                                         (log-msg (format "%s-%s" msg call-count))
                                         (log-msg (format "%s-%s" msg call-count)))]

        (log-every-other "1 message")
        (is (= [] @msgs-logged) )

        (log-every-other "another message")
        (is (= ["another message-2" "another message-2"] @msgs-logged))

        (log-every-other "3rd message")
        (is (= ["another message-2" "another message-2"] @msgs-logged))

        (log-every-other "4th")
        (is (= ["another message-2" "another message-2" "4th-4" "4th-4"] @msgs-logged))))))

(deftest test-timestamp?
  (are [n result] (= (timestamp? n) result)
    nil      false
    -4444444 false
    -333333  false
    -10000   false
    -999     false
    -77      false
    -5       false
    -3       false
    -2       false
    -1       false
    0        true
    1        true
    2        true
    3        true
    5        true
    77       true
    999      true
    10000    true
    333333   true
    4444444  true
    0.0        false
    1.0        false
    2.0        false
    3.0        false
    5.0        false
    77.0       false
    999.0      false
    10000.0    false
    333333.0   false
    4444444.0  false
    9223372036854775807 true
    9223372036854775808 false))

(def throws-on-1st-or-2nd-call
  (let [cnt (atom 3)]
    (fn []
      (swap! cnt dec)
      (if (< @cnt 1)
        :foo
        (raise "BOOM!")))))

(deftest test-with-retries
  (is (= :foo (with-retries 3 (throws-on-1st-or-2nd-call))))
  (is (thrown? Exception (with-retries 3 (raise Exception "BLAMMO!")))))

(let [name-maker (incremental-name-with-prefix "name")]
  
  (deftest test-incremental-name-with-prefix
    (is (= "name-0" (name-maker)))
    (is (= "name-1" (name-maker)))
    (is (= "name-2" (name-maker)))))


(deftest test-make-comparator
  (is (= 1 ((make-comparator < :key-fn :id) {:name "foo" :id 2} {:name "bar" :id 1})))
  (is (= 0 ((make-comparator < :key-fn :id) {:name "foo" :id 2} {:name "foo" :id 2})))
  (is (= -1 ((make-comparator < :key-fn :id) {:name "bar" :id 1} {:name "foo" :id 2}))))

(deftest test-div
  (is (= 0.5 (div 1 2)))
  (is (= 0.5 (div 1.0 2.0)))
  (is (= nil (div 100 0))))

(deftest test-parse-cents
  (is (= nil (parse-cents nil)))
  (is (= 199 (parse-cents "1.99")))
  (is (= true (long? (parse-cents "1.99"))))

  (is (= 199 (parse-cents "1.992")))
  (is (thrown? Exception (parse-cents :not-a-string))))

(deftest test-average
  (is (= 3 (average 2 3 4)))
  (is (= nil (average))))

(deftest test-long?
  (is (= true (long? (long 123))))
  (is (= false (long? 1.99))))

(deftest test-blank->nil
  (is (= nil (blank->nil nil)))
  (is (= nil (blank->nil "")))
  (is (= "   " (blank->nil "   ")))
  (is (= 222 (blank->nil 222))))

(deftest test-ensure-long
  (is (= (long 5) (ensure-long (int 5))))
  (is (= true (long? (ensure-long (int 5)))))

  (is (= (long 5) (ensure-long (long 5))))
  (is (= true (long? (ensure-long (long 5)))))
  
  (is (= (long 5) (ensure-long "5")))
  (is (= true (long? (ensure-long "5")))))

(deftest test-single-destructuring-arg->form+name
  (stubbing [gensym 'unique-3]
    (are [original form name] (let [[frm nm] (single-destructuring-arg->form+name original)]
                                (and (= frm form)
                                  (= nm name)))
      'a                     'a                          'a
      '[a b]                 '[a b :as unique-3]         'unique-3
      '[a b & c :as all]     '[a b & c :as all]          'all
      '{:keys [a b]}         '{:keys [a b] :as unique-3} 'unique-3
      '{:keys [a b] :as all} '{:keys [a b] :as all}      'all
      ;; pathological cases
      '[a]                   '[a :as unique-3]           'unique-3
      '[a :as b]             '[a :as b]                  'b)))

(deftest test-read-string-securely
  (is (= '(list 1 2 3)
         (read-string-securely "(list 1 2 3)")))
  (is (= nil (read-string-securely nil)))
  (is (thrown? RuntimeException (read-string-securely "#=(eval (def x 3))"))))

(defn-kw my-fn [one two & {:keys [k1 k2] :as opts}]
  (+ one
     two 
     (if k1 k1 0)
     (if k2 k2 0)))

(deftest test-defn-kw
  (is (= 22 (my-fn 1 3 :k1 7 :k2 11)))
  (is (= 4 (my-fn 1 3)))

  (is (thrown-with-msg? AssertionError
        #"defn-kw expects the final element of the arg list to be a map destructuring."
        (eval `(defn-kw f [a b c] nil))))

  (is (thrown-with-msg? AssertionError
        #"defn-kw expects the map destructuring to have a :keys key."
        (eval `(defn-kw f [a b c & {:doorknobs [d e]}] nil))))

  (is (thrown-with-msg? AssertionError
        #"defn-kw expects the second to last element of the arg list to be an '&"
        (eval `(defn-kw f [a b c not-ampersand {:keys [d e]}] nil))))
  
  (is (thrown-with-msg? AssertionError #"Was passed these keyword args #\{:k9999\} which were not listed in the arg list \[one two & \{:keys \[k1 k2\], :as opts\}\]"
        (my-fn 1 2 :k9999 4))))
