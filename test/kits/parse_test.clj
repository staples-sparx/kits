(ns kits.parse-test
  (:require  [kits.parse         :as parse] 
             [kits.test-utils    :as tst] )
  (:use      [clojure.test] )
  (:import   [java.lang.Math] )
)

(set! *warn-on-reflection* false)

(deftest string->number
  (testing "Parsing str->byte"
    (is (= 15                              (parse/str->byte "15")))
    (is (= -5                              (parse/str->byte "-5")))
    (is (thrown? NumberFormatException     (parse/str->byte "999")))
    (is (thrown? NumberFormatException     (parse/str->byte " "))) )

  (testing "Parsing str->short"
    (is (= 15                              (parse/str->short "15")))
    (is (= -5                              (parse/str->short "-5")))
    (is (= 999                             (parse/str->short "999")))
    (is (thrown? NumberFormatException     (parse/str->short "99999")))
    (is (thrown? NumberFormatException     (parse/str->short" "))) )

  (testing "Parsing str->int"
    (is (= 15                              (parse/str->int "15")))
    (is (= -5                              (parse/str->int "-5")))
    (is (= 99999                           (parse/str->int "99999")))
    (is (thrown? NumberFormatException     (parse/str->int "9876543210")))
    (is (thrown? NumberFormatException     (parse/str->int " "))) )

  (testing "Parsing str->long"
    (is (= 15                              (parse/str->long "15")))
    (is (= -5                              (parse/str->long "-5")))
    (is (= 99999                           (parse/str->long "99999")))
    (is (= 9876543210                      (parse/str->long "9876543210")))
    (is (thrown? NumberFormatException     (parse/str->long "98765432109876543210")))
    (is (thrown? NumberFormatException     (parse/str->long " "))) )

  (testing "Parsing str->float"
    (is (= 15.0                            (parse/str->float "15")))
    (is (= -5.0                            (parse/str->float "-5")))
    (is (thrown? NumberFormatException     (parse/str->float " ")))
    (is (= 0.5                             (parse/str->float "0.5")))
    (is (> 1e-6 (tst/error-ratio           (parse/str->float "0.1") 0.1 )))
    (is (> 1e-6 (tst/error-ratio           (parse/str->float "3.141592654") 3.141592654 ))) )

  (testing "Parsing str->double"
    (is (= 15.0                            (parse/str->double "15")))
    (is (= -5.0                            (parse/str->double "-5")))
    (is (thrown? NumberFormatException     (parse/str->double " ")))
    (is (= 0.5                             (parse/str->double "0.5")))
    (is (> 1e-6 (tst/error-ratio           (parse/str->double "0.1") (double (/ 1 10) ))))
    (is (> 1e-6 (tst/error-ratio           (parse/str->double "3.141592654") Math/PI   ))) ))


