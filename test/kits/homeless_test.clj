(ns kits.homeless-test
  (:use
    kits.foundation
    clojure.test)
  (:require
    [kits.homeless :as h]
    [kits.test-utils :as u]))

(deftest all-kits.namespaces-have-doc-strings
  (testing "Since Kits is a set of core libraries for Runa, we want things to
            be well documented.  Each namespace should have a clear Single
            Responsibility explained in its doc string."
    (is (= [] (->> (all-ns)
                   (filter #(.startsWith (str (ns-name %)) "kits."))
                   (remove #(.startsWith (str (ns-name %)) "kits.test"))
                   (remove #(.endsWith   (str (ns-name %)) "-test"))
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
  (are [expected str default] (= expected (h/parse-number str default))
       nil nil nil
       10 "10" nil
       nil "" nil
       24 "" 24
       10.0 "10.00" nil
       10.0 "10.00" 0
       10 10 0))

(deftest test-boolean?
  (are [x bool?] (= bool? (h/boolean? x))
       false true
       true true
       nil  false
       "ad" false
       []   false
       {}   false))

(deftest test-wrap-trapping-errors
  (is (false? ((h/wrap-trapping-errors pos? false) "string")))
  (is (nil? ((h/wrap-trapping-errors pos?) "string")))
  (is (true?  ((h/wrap-trapping-errors string?) "string"))))

(deftest test-ip-address-v4?
  (is (false? (h/ip-address-v4? "34.342.3.4")))
  (is (h/ip-address-v4? "192.168.0.1")))

(deftest test-str->boolean
  (is (h/str->boolean "true"))
  (is (false? (h/str->boolean "")))
  (is (false? (h/str->boolean "false"))))

(deftest test-ipv4-dotted-to-integer
  (is (= (h/ipv4-dotted-to-integer "127.0.0.1") 2130706433)))

(deftest test-ipv4-integer-to-dotted
  (is (= "127.0.0.1" (h/ipv4-integer-to-dotted
                       (h/ipv4-dotted-to-integer "127.0.0.1")))))

(deftest test-parse-url
  (is (= {:scheme "http", :host "www.runa.com", :path "/design"} (h/parse-url "http://www.runa.com/design")))
  (is (= nil (h/parse-url "")))
  (is (= nil (h/parse-url nil))))

(deftest test-url?
  (is (false? (h/url? "malformedhttp:// url")))
  (is (false? (h/url? "")))
  (is (false? (h/url? nil)))
  (is (h/url? "http://www.runa.com/")))

(deftest test-periodic-fn
  (let [msgs-logged (atom [])
        log-msg (fn [msg]
                  (swap! msgs-logged conj msg))]
    (testing "creates a fn that only gets call every period times (2 in this example);
            has access to the 'call-count' as well, and the fn body is an implicit 'do'"
      (let [log-every-other (h/periodic-fn [msg] [call-count 2]
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
  (are [n result] (= (h/timestamp? n) result)
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

(defn retry-handler [opts t retry-count]
  )

(defn fail-handler [opts t]
  )

(deftest test-with-retries
  (is (thrown? Exception
        (h/with-retries {:max-times 1}
          (throws-on-1st-or-2nd-call))))

  (testing "handles map options or integer max-times"
    (is (= :foo (h/with-retries {:max-times 3}
                  (throws-on-1st-or-2nd-call))))

    (is (= :foo (h/with-retries 3
                  (throws-on-1st-or-2nd-call)))))

  (is (thrown? Exception
        (h/with-retries {:max-times 3
                       :retry-handler retry-handler
                       :fail-handler fail-handler}
          (raise Exception "BLAMMO!"))))

  (is (thrown? Exception
        (h/with-retries {:max-times 3
                       :retry-handler retry-handler
                       :fail-handler fail-handler}
          (raise Exception "BLAMMO!"))))

  (is (not-thrown? Exception
        (h/with-retries {:max-times 3
                       :retry-handler retry-handler
                       :fail-handler fail-handler
                       :swallow-exceptions? true}
          (raise Exception "BLAMMO!")))))

(deftest test-incremental-name-with-prefix
  (let [name-maker1 (h/incremental-name-with-prefix "name")]
    (is (= "name-0" (name-maker1)))
    (is (= "name-1" (name-maker1)))
    (is (= "name-2" (name-maker1)))))


(deftest test-make-comparator
  (is (= 1 ((h/make-comparator < :key-fn :id) {:name "foo" :id 2} {:name "bar" :id 1})))
  (is (= 0 ((h/make-comparator < :key-fn :id) {:name "foo" :id 2} {:name "foo" :id 2})))
  (is (= -1 ((h/make-comparator < :key-fn :id) {:name "bar" :id 1} {:name "foo" :id 2}))))

(deftest test-div
  (is (= 0.5 (h/div 1 2)))
  (is (= 0.5 (h/div 1.0 2.0)))
  (is (= nil (h/div 100 0))))

(deftest test-parse-cents
  (is (= nil (h/parse-cents nil)))
  (is (= 199 (h/parse-cents "1.99")))
  (is (= true (long? (h/parse-cents "1.99"))))

  (is (= 199 (h/parse-cents "1.992")))
  (is (thrown? Exception (h/parse-cents :not-a-string))))

(deftest test-average
  (is (= 3 (h/average 2 3 4)))
  (is (= nil (h/average))))

(deftest test-long?
  (is (= true (long? (long 123))))
  (is (= false (long? 1.99))))

(deftest test-blank->nil
  (is (= nil (h/blank->nil nil)))
  (is (= nil (h/blank->nil "")))
  (is (= "   " (h/blank->nil "   ")))
  (is (= 222 (h/blank->nil 222))))

(deftest test-ensure-long
  (is (= (long 5) (h/ensure-long (int 5))))
  (is (= true (long? (h/ensure-long (int 5)))))

  (is (= (long 5) (h/ensure-long (long 5))))
  (is (= true (long? (h/ensure-long (long 5)))))

  (is (= (long 5) (h/ensure-long "5")))
  (is (= true (long? (h/ensure-long "5")))))

(deftest test-single-destructuring-arg->form+name
  (with-redefs [gensym (constantly 'unique-3)]
    (are [original form name] (let [[frm nm] (h/single-destructuring-arg->form+name original)]
                                (and (= frm form)
                                  (= nm name)))

      'a                     'a                          'a
      '[a b]                 '[a b :as unique-3]         'unique-3
      '[a b & c :as all]     '[a b & c :as all]          'all
      '{:keys [a b]}         '{:keys [a b] :as unique-3} 'unique-3
      '{:keys [a b] :as all} '{:keys [a b] :as all}      'all
      ;; pathological cases
      '[a]                   '[a :as unique-3]           'unique-3
      '[a :as b]             '[a :as b]                  'b

      )))

(deftest test-read-string-securely
  (is (= '(list 1 2 3)
         (h/read-string-securely "(list 1 2 3)")))
  (is (= nil (h/read-string-securely nil)))
  (is (thrown? RuntimeException (h/read-string-securely "#=(eval (def x 3))"))))

(h/defn-kw my-fn [one two & {:keys [k1 k2] :as opts}]
  (+ one
     two
     (if k1 k1 0)
     (if k2 k2 0)))

(h/defn-kw ^:private doc-string-fn
  "def-kw w/ doc string"
  [& {:keys [k1 k2]}]
  (+ k1 k2))

(deftest test-defn-kw
  (is (= 22 (my-fn 1 3 :k1 7 :k2 11)))
  (is (= 4 (my-fn 1 3))) ;; testing it works w/ no keyword args passed in

  (is (thrown-with-msg? AssertionError
                        #"defn-kw expects the final element of the arg list, .*, to be a map destructuring."
                        (eval `(h/defn-kw f [a b c] nil))))

  (is (thrown-with-msg? AssertionError
                        #"defn-kw expects the map destructuring to have a :keys or :strs key."
                        (eval `(h/defn-kw f [a b c & {:doorknobs [d e]}] nil))))

  (is (thrown-with-msg? AssertionError
                        #"defn-kw expects the second to last element of the arg list, .*, to be an '&"
                        (eval `(h/defn-kw f [a b c not-ampersand {:keys [d e]}] nil))))

  (is (thrown-with-msg? AssertionError #"Was passed these keyword args #\{:k9999\} which were not listed in the arg list \[one two & \{:keys \[k1 k2\], :as opts\}\]"
                        (my-fn 1 2 :k9999 4)))

  (is (= "def-kw w/ doc string" (:doc (meta #'doc-string-fn))))
  (is (= true (:private (meta #'doc-string-fn)))))


(h/defn-kw my-fn2 [one two & {:strs [k1 k2] :as opts}]
  (+ one
     two
     (if k1 k1 0)
     (if k2 k2 0)))

(h/defn-kw ^:private doc-string-fn2
  "def-kw w/ doc string 2"
  [& {:strs [k1 k2]}]
  (+ k1 k2))

(deftest test-defn-kw-with-strs-destructuring
  (is (= 22 (my-fn2 1 3 "k1" 7 "k2" 11)))
  (is (= 4 (my-fn2 1 3))) ;; testing it works w/ no keyword args passed in

  (is (thrown-with-msg? AssertionError
                        #"defn-kw expects the final element of the arg list, .*, to be a map destructuring."
                        (eval `(h/defn-kw f [a b c] nil))))

  (is (thrown-with-msg? AssertionError
                        #"defn-kw expects the map destructuring to have a :keys or :strs key."
                        (eval `(h/defn-kw f [a b c & {"doorknobs" [d e]}] nil))))

  (is (thrown-with-msg? AssertionError
                        #"defn-kw expects the second to last element of the arg list, .*, to be an '&"
                        (eval `(h/defn-kw f [a b c not-ampersand {:strs [d e]}] nil))))

  (is (thrown-with-msg? AssertionError #"Was passed these keyword args #\{\"k9999\"\} which were not listed in the arg list \[one two & \{:strs \[k1 k2\], :as opts\}\]"
                        (my-fn2 1 2 "k9999" 4)))

  (is (= "def-kw w/ doc string 2" (:doc (meta #'doc-string-fn2))))
  (is (= true (:private (meta #'doc-string-fn2)))))

(defn kw-fn [a b & {:keys [c d]}]
  (str a b c d))

(deftest test-apply-kw
  (is (= "abcd" (h/apply-kw kw-fn "a" "b" {:c "c" :d "d"}))))

(deftest test-within?
  (is (= true (h/within? 10 5 15)))
  (is (= true (h/within? 10 5 14)))
  (is (= false (h/within? 10 5 16))))

(deftest test-approximately-equal?
  (is (h/approximately-equal? 0.0 0.0))
  (is (h/approximately-equal? 10.0 10.0))
  (is (h/approximately-equal? -10.0 -10.0))
  (is (h/approximately-equal? 10.0 10.00001))
  (is (not (h/approximately-equal? 10.0 10.00001 0.0000001)))
  (is (not (h/approximately-equal? 10.0 10.001)))
  (is (h/approximately-equal? Double/NaN 0.0))
  (is (h/approximately-equal? 0.0 Double/NaN))
  (is (not (h/approximately-equal? 10.0 -10.0)))
  (is (h/approximately-equal? 10.0 10)))
