(ns kits.csv-test
  (:use clojure.test)
  (:require [clojure.java.io :as io]
            [kits.csv :as csv]
            [kits.foundation :as f]))

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

        id-1-p-result
        (csv/csv-rows->coll result (assoc sample-field-reader-opts :pred-fn #(= (:id %) 1)))]

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

    (testing "Given a csv and a predicate function, filter the collection of maps by the predicate function"
      (is (= [(get expected-map-result 1)]
             id-1-p-result)))

    (testing "The parsed csv does not contain empty lines"
      (is (empty? (->> coll-result (filter :id) (filter nil?)))))))

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

(def sample-psv (let [psv "/tmp/sample-psv.psv"
                      _ (spit "/tmp/sample-psv.psv"
"PERSON_ID|bc_seg|ZIP_POSTAL_CODE
1427447|Holdout Runa|03903
1428540|Holdout Runa|45231
1428694|Holdout Runa|18104
1428727|Holdout Runa|33610
1429587|Holdout Runa|17856
1429905|Holdout Runa|50480
1430343|Holdout Runa|28270
1431186|Holdout Runa|11580
1431413|Holdout Runa|03062")]
                  psv))

(deftest load-psv-files
  (testing "loading of psv files"
    (is (= {:zip "03062", :id "1431413"}
           (get (with-open [rdr (io/reader sample-psv)]
                  (csv/csv-rows->map (csv/read-csv
                                      rdr
                                      {:skip-header true :delimiter \|})
                                     {:key-fn :id
                                      0 {:label :id :reader identity}
                                      2 {:label :zip :reader identity}}))
                "1431413")))))

