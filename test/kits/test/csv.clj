(ns kits.test.csv
  (:use clojure.test)
  (:require
   [kits.csv :as csv]
   [kits.foundation :as f]))


(def sample-csv (str (System/getProperty "user.dir")
                     "/samples/sample.csv"))
(def sample-field-reader-opts
  {:key-fn :id
   :val-fn identity
   0 {:label :id :reader f/parse-int}
   1 {:label :left :reader f/parse-int}
   2 {:label :right :reader f/parse-int}
   3 {:label :split_var :reader identity}
   4 {:label :split_point :reader identity}
   5 {:label :status :reader identity}
   6 {:label :prediction :reader identity}})

(deftest parsing-csv-into-nested-maps
  (testing "Given a csv file, generate map of maps {k-fn, the row} "
    (is (= {1
            {:status "1",
             :split_point "1990",
             :prediction "NA",
             :split_var "most_expensive_product_in_cart",
             :right 3,
             :left 2,
             :id 1},
            2
            {:status "1",
             :split_point "0.5",
             :prediction "NA",
             :split_var "os.Windows",
             :right 5,
             :left 4,
             :id 2},
            3
            {:status "1",
             :split_point "1.5",
             :prediction "NA",
             :split_var "cart_actions",
             :right 7,
             :left 6,
             :id 3},
            4
            {:status "1",
             :split_point "8983.5",
             :prediction "NA",
             :split_var "price",
             :right 9,
             :left 8,
             :id 4}}
           (csv/csv-rows->map (csv/read-csv sample-csv {:skip-header true :delimiter \space})
                            sample-field-reader-opts)))))