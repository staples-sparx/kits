(ns kits.test.csv
  (:use clojure.test)
  (:require
   [kits.csv :as csv]
   [clojure.java.io :as io]
   [kits.foundation :as f])
  (:import java.util.HashMap
           java.util.ArrayList))

(def sample-csv (str (System/getProperty "user.dir")
                     "/samples/sample.csv"))

(def sample-field-reader-opts
  {:key-fn :id
   :val-fn (fn [row] (assoc row :extra "added this"))
   0 {:label :id :reader f/parse-int}
   1 {:label :left :reader f/parse-int}
   2 {:label :right :reader f/parse-int}
   3 {:label :split_var :reader identity}
   4 {:label :split_point :reader identity}
   5 {:label :status :reader identity}
   6 {:label :prediction :reader identity}})


(def sample-field-reader-opts-mutable
  (assoc sample-field-reader-opts
    :mutable? true
    :val-fn (fn [row]
              (.put ^HashMap row :extra "added this")
              row)))

(def expected-map-result
  {1
   {:status "1",
    :split_point "1990",
    :prediction "NA",
    :split_var "most_expensive_product_in_cart",
    :right 3,
    :left 2,
    :extra "added this",
    :id 1},
   2
   {:status "1",
    :split_point "0.5",
    :prediction "NA",
    :split_var "os.Windows",
    :right 5,
    :left 4,
    :extra "added this",
    :id 2},
   3
   {:status "1",
    :split_point "1.5",
    :prediction "NA",
    :split_var "cart_actions",
    :right 7,
    :left 6,
    :extra "added this",
    :id 3},
   4
   {:status "1",
    :split_point "8983.5",
    :prediction "NA",
    :split_var "price",
    :right 9,
    :left 8,
    :extra "added this",
    :id 4}})

(def expected-coll-result
  [(get expected-map-result 1)
   (get expected-map-result 2)
   (get expected-map-result 3)
   (get expected-map-result 4)])

(deftest parsing-csv-into-nested-maps
  (let [result
        (with-open [rdr (io/reader sample-csv)]
          (doall (csv/read-csv rdr
                               {:skip-header true
                                :delimiter \space})))
        map-result
        (csv/csv-rows->map result sample-field-reader-opts)

        coll-result
        (csv/csv-rows->coll result sample-field-reader-opts)

        mutable-result
        (with-open [rdr (io/reader sample-csv)]
          (doall (csv/read-csv
                  rdr
                  {:skip-header true
                   :delimiter \space
                   :mutable? true})))

        #_mutable-map-result ; TODO maybe?
        #_(csv/csv-rows->map mutable-result
                             sample-field-reader-opts-mutable)

        mutable-coll-result
        (csv/csv-rows->coll mutable-result
                            sample-field-reader-opts-mutable)]

    (testing "Given a csv, generate map of maps {k-fn, the row}"
      (is (= expected-map-result
             map-result))
      (is (map? map-result))
      (is (map? (get map-result 1))))

    (testing "Given a csv, generate a coll of maps {k-fn, the row}"
      (is (= expected-coll-result
             coll-result))
      (is (seq? coll-result))
      (is (map? (first coll-result))))

    (comment ; TODO maybe?
      (testing
          "Given a csv, generate mutable map of mutable maps {k-fn, the row}"
        (is (= (get expected-map-result 1)
               (first (.values mutable-map-result))))
        (is (= java.util.HashMap
               (type (first (.values mutable-map-result)))))
        (is (= java.util.HashMap
               (type mutable-map-result)))))

    (testing
        "Given a csv, generate mutable array of mutable maps {k-fn, the row}"
      (is (= expected-coll-result
             (seq mutable-coll-result)))
      (is (= java.util.HashMap
             (type (first mutable-coll-result))))
      (is (= java.util.ArrayList
             (type mutable-coll-result))))))

(deftest parsing-csv-into-nested-maps-with-columns-excluded
  (testing
      "Given a csv, generate map of maps {k-fn, the row} excludes columns"
    (is (= {1
            {:status "1",
             :split_point "1990",
             :split_var "most_expensive_product_in_cart",
             :right 3,
             :extra "added this",
             :id 1},
            2
            {:status "1",
             :split_point "0.5",
             :split_var "os.Windows",
             :right 5,
             :extra "added this",
             :id 2},
            3
            {:status "1",
             :split_point "1.5",
             :split_var "cart_actions",
             :right 7,
             :extra "added this",
             :id 3},
            4
            {:status "1",
             :split_point "8983.5",
             :split_var "price",
             :right 9,
             :extra "added this",
             :id 4}}
           (with-open [rdr (io/reader sample-csv)]
             (csv/csv-rows->map (csv/read-csv
                                 rdr
                                 {:skip-header true :delimiter \space})
                                (assoc sample-field-reader-opts
                                  :exclude-columns #{1 6})))))))
