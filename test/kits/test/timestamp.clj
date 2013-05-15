(ns kits.test.timestamp
  (:use clojure.test
        kits.timestamp))


(deftest test->timestamp-at-day-end
  (are [t expected] (= (->timestamp-at-day-end t) (->timestamp expected))
       "2012-05-01"  "2012-05-01 23:59:59"
       "2012-05-01 00:00" "2012-05-01 23:59:59"
       "2012-05-01 00:00:00" "2012-05-01 23:59:59"
       1335830400000 "2012-05-01 23:59:59"))

(deftest test->timestamp-at-day-start
  (are [t expected] (= (->timestamp-at-day-start t) (->timestamp expected))
       "2012-05-01" "2012-05-01 00:00:00"
       "2012-05-01 11:11" "2012-05-01 00:00:00"     
       "2012-05-01 05:55:55" "2012-05-01 00:00:00"
       1335873634000 "2012-05-01 00:00:00"))

(deftest test-truncate
  (are [t interval expected] (= (->str (truncate (->timestamp t) interval)) expected)
       "2012-05-03 22:22:22" :second "2012-05-03 22:22:22"
       "2012-05-03 22:22:22" :minute "2012-05-03 22:22:00"
       "2012-05-03 22:22:22" :hour "2012-05-03 22:00:00"
       "2012-05-03 22:22:22" :day "2012-05-03 00:00:00"
       "2012-05-03 22:22:22" :month "2012-05-01 00:00:00"
       "2012-05-03 22:22:22" :year "2012-01-01 00:00:00"))

(deftest test->timestamp
  (are [t expected] (= (->str (->timestamp t)) expected)
       "2012-05-01" "2012-05-01 00:00:00"
       "2012-05-01 12:12" "2012-05-01 12:12:00"
       "2012-05-01 12:12:12" "2012-05-01 12:12:12"
       "1335830400000" "2012-05-01 00:00:00"
       1335830400000 "2012-05-01 00:00:00")
  (are [t] (thrown-with-msg? IllegalArgumentException #"Don't know how to parse" (->timestamp t))
       nil
       "cat"
       "1.5"
       ""
       1.5
       1M))

(deftest test->timestamp---with-specified-format
  (is (= "2012-01-30 00:00:00" (->str (->timestamp "01 30, 2012" "MM dd, yyyy")))))

(deftest test->timestamp---integral-types-get-coerced-to-Long
  (is (= true (instance? Long (->timestamp (int 5)))))
  (is (= true (instance? Long (->timestamp (short 5))))))

(deftest test-timestamp-ranges
  
  (testing "Calendar units are (milli second minute hour day month week year)"
    
    (testing "There is no support for millisecond/second ranges."
      (is (thrown? IllegalArgumentException
                   (timestamp-ranges "2000-01-01 12:00:01" "2000-01-01 12:00:10" :milli)))
      (is (thrown? IllegalArgumentException
                   (timestamp-ranges "2000-01-01 12:00:01" "2000-01-01 12:00:10" :second))))
    
    
    (testing "Function timestamp-ranges works only for: "

      (testing "minute ranges"
        (is (= (->> (timestamp-ranges "2000-01-01 12:01" "2000-01-01 12:03" :minute)
                    (apply concat)
                    (map ->str))
               ["2000-01-01 12:01:00" "2000-01-01 12:01:59"
                "2000-01-01 12:02:00" "2000-01-01 12:02:59"
                "2000-01-01 12:03:00" "2000-01-01 12:03:59"])))
      
      (testing "hour ranges"
        (is (= (->> (timestamp-ranges "2000-01-01 12:00" "2000-01-01 14:00" :hour)
                    (apply concat)
                    (map ->str))
               ["2000-01-01 12:00:00" "2000-01-01 12:59:59"
                "2000-01-01 13:00:00" "2000-01-01 13:59:59"
                "2000-01-01 14:00:00" "2000-01-01 14:59:59"])))
    
      (testing "day ranges"
        (is (= (->> (timestamp-ranges "2000-01-01" "2000-01-03" :day)
                    (apply concat)
                    (map ->str))
               ["2000-01-01 00:00:00" "2000-01-01 23:59:59"
                "2000-01-02 00:00:00" "2000-01-02 23:59:59"
                "2000-01-03 00:00:00" "2000-01-03 23:59:59"])))
    
      (testing "week ranges"
        (is (= (->> (timestamp-ranges "2000-01-01" "2000-01-24" :week)
                    (apply concat)
                    (map ->str))
               ["2000-01-01 00:00:00" "2000-01-07 23:59:59"
                "2000-01-08 00:00:00" "2000-01-14 23:59:59"
                "2000-01-15 00:00:00" "2000-01-21 23:59:59"
                "2000-01-22 00:00:00" "2000-01-28 23:59:59"])))

      (testing "month ranges"
        (is (= (->> (timestamp-ranges "2000-01-01" "2000-03-02" :month)
                    (apply concat)
                    (map ->str))
               ["2000-01-01 00:00:00" "2000-01-31 23:59:59"
                "2000-02-01 00:00:00" "2000-02-29 23:59:59"
                "2000-03-01 00:00:00" "2000-03-02 23:59:59"])))

      (testing "year ranges"
        (is (= (->> (timestamp-ranges "2000-01-01" "2003-03-02" :year)
                    (apply concat)
                    (map ->str))
               ["2000-01-01 00:00:00" "2000-12-31 23:59:59"
                "2001-01-01 00:00:00" "2001-12-31 23:59:59"
                "2002-01-01 00:00:00" "2002-12-31 23:59:59"
                "2003-01-01 00:00:00" "2003-12-31 23:59:59"])))

      (testing "no range"
        (is (= (timestamp-ranges "2013-05-01" "2013-05-31" :none)
               [[1367366400000 1369958400000]]))))))


(deftest test-add
  (is (= (add (->timestamp "2012-05-21") :day 4)
         (->timestamp "2012-05-25"))))

(deftest test-subtract
  (is (= (subtract (->timestamp "2012-05-25") :day 4)
         (->timestamp "2012-05-21"))))

(deftest test-->str-with-optional-format-specfied
  (is (= "2012/05/21"
         (->str (->timestamp "2012-05-21") "yyyy/MM/dd"))))

(deftest test-specifying-timezone
  (is (= 28800000 ;; 8 hours
         (- (->timestamp "03/01/2012" "MM/dd/yyyy" "PST")
            (->timestamp "03/01/2012" "MM/dd/yyyy" "UTC")))))

