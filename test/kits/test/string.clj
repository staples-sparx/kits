(ns kits.test.string
  (:use clojure.test
        kits.string))


(deftest test-keyword-munging
  (is (= "a-foo-1" (keyword->hyphenated-string :a_foo_1)))
  (is (= :a-foo-1 (keyword->hyphenated-keyword :a_foo_1)))
  (is (= "a_foo_1" (keyword->underscored-string :a-foo-1)))
  (is (= :a_foo_1 (keyword->underscored-keyword :a-foo-1))))


(deftest ^:unit test-decode-string
  (are [expected s] (= expected (decode-string s))
       
       "regular"                 "regular"
       "regular_user"            "regular+user"
       "regular_user_siz_"       "regular+user+siz%3A"
       "1"                       1))

(deftest ^:unit test-urldecode
  (are [x y] (= x y)
       "regular" (urldecode "regular")
       "regular user" (urldecode "regular+user")
       "regular user siz:" (urldecode "regular+user+siz%3A")))

(deftest ^:unit test-escape-regex
  (are [x y] (= x y)
       "\\[a\\.b\\?\\]" (escape-regex-except-* "[a.b?]")
       "a.*b" (escape-regex-except-* "a*b")))

(deftest ^:unit test-query-string
  (are [expected url] (= expected (query-string url))
       nil ""
       nil nil
       nil "http://www.google.com"
       nil " http://www.google.com "
       nil " www.google.com "
       "sa=t&source=web&cd=1&ved=0CBsQFjAA&url=http://www.presidentsolitaire.com/&rct=j" "http://www.google.com/url?sa=t&source=web&cd=1&ved=0CBsQFjAA&url=http://www.presidentsolitaire.com/&rct=j"))

(deftest ^:unit test-parse-query-param
  (let [q "sa=t&source=web&cd=1&ved=0CBsQFjAA&url=http://www.presidentsolitaire.com/&rct=j&q=president solitaire&ei=vGGQTNjiEoj2tgOKp8SxDg&usg=AFQjCNEgO94CXSKxjr7vRxT5R4dQXu_XLQ"
        qm (parse-query-param q)]
    (is (= (count qm) 9))
    (is (= (qm "q") "president solitaire"))))

(deftest ^:unit test-parse-query-param-empty
  (let [q "sa=t&source=web&cd=1&ved=0CBsQFjAA&url=http://www.presidentsolitaire.com/&rct=j&q=&ei=vGGQTNjiEoj2tgOKp8SxDg&usg=AFQjCNEgO94CXSKxjr7vRxT5R4dQXu_XLQ"
        qm (parse-query-param q)]
    (is (= (count qm) 9))
    (is (= (qm "q") ""))))

(deftest ^:unit test-parse-query-param-in-nil-case
  (let [qm (parse-query-param nil)]
    (is (= {} qm ))))


(deftest ^:unit test-domain-name
  (is (= "yahoo" (domain-name "www.yahoo.com")))
  (is (= "yahoo" (domain-name "search.blah.asda.yahoo.com")))
  (is (= "yahoo" (domain-name "yahoo"))))


(deftest ^:unit test-extract-number
  (are [expected s] (= expected (extract-number s))

       12.23   "$12.23"
       12.23   "$12.23Testn"
       12.23   " 12.23 "
       12.23   "some junk 12.23 ok this is not"
       12.23   12.23
       -12.23  "-12.23"
       -12.23   "abc-12.23"
       -23      "12-23"))

(deftest ^:unit hyphenize-replaces-all-underscores-with-hyphens
  (are [result arg] (= result (hyphenize arg))
    nil nil
    "" ""
    "foo-bar"     "foo-bar"
    "foo-bar"     "foo_bar"
    "-foo-Bar-Baz-" "_foo_Bar_Baz_"))

(deftest ^:unit decamelize-lowercases-and-splits-word-boundaries-with-hyphens
  (are [result arg] (= result (decamelize arg))
    nil nil
    "" ""
    "foo-bar"      "foo-bar"
    "foo-bar"      "FooBar"
    "foo-bar"      "fooBar"

    ;; only decamelizes letters of the alphabet
    "f001bar2baz3" "f001Bar2Baz3"))

(deftest ^:unit test-all-whitespace?
  (are [result s] (= result (all-whitespace? s))
    false     nil
    true      ""
    true      "    "
    true      "  \t \r \n "))

(deftest ^:unit splits-string-using-delimiter
  (are [result s delim] (= result (split s delim))

    ;; splits on a regex delimiter
    nil                nil            #","
    ["foo" "bar"]      "foo,bar"      #","
    [" foo " " bar "]  " foo : bar "  #":"

    ;; also, accepts string for delimiter
    nil                nil            ","
    ["foo" "bar"]      "foo,bar"      ","
    [" foo " " bar "]  " foo : bar "  ":"))

(deftest ^:unit test-nil-str
  (are [result x] (= result (nil-str x))
    nil     nil
    "foo"   "foo"
    "1"      1
    "c"      \c
    "1.0"    1.0
    "[1 2]" [1 2]
    "(:foo :bar)" (list :foo :bar)
    "clojure.lang.PersistentList$EmptyList@1" '()))

(deftest ^:unit test-empty-str
  (are [result x] (= result (empty-str x))
       nil     nil
       nil     ""
       "foo"   "foo"
       "1"      1
       "c"      \c
       "1.0"    1.0
       "[1 2]" [1 2]
       "(:foo :bar)" (list :foo :bar)
       nil     '()))

(deftest ^:unit test-git-sha?
  (are [x sha?] (= sha? (git-sha? x))
    nil false
    "c15d29f6b4fcefc5946dc4b8d7290669fc20d961" true    ;; just right :)
    "c15d29f6b4fcefc5946dc4b8d7290669fc20d96" false    ;; too short
    "c15d29f6b4fcefc5946dc4b8d7290669fc20d9611" false  ;; too long
    "c15d29f6b4fcegc5946dc4b8d7290669fc20d961" false)) ;; 'g' not valid

(deftest ^:unit test-uuid?
  (are [x result] (= (uuid? x) result)
    nil false
    ""  false
    "e5b03c68-318f-4072-a626-e3f423a2b382" true))


;; Keyword function tests

(deftest ^:unit test-underscore->hyphens
  (are [result input] (= result (underscores->hyphens input))
    nil nil
    :-a-b-c- :_a_b_c_))

(deftest ^:unit test-hyphens->underscores
  (are [result input] (= result (hyphens->underscores input))
    nil nil
    :_a_b_c_ :-a-b-c-))

(deftest ^:unit test-remove-dashes
  (is (= (remove-dashes "123-2451") "1232451")))