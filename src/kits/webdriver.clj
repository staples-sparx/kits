(ns kits.webdriver
  (:require [clj-webdriver.taxi :as wd]))


(defn extract-html-table
  "Extracts a table from a webpage, representing it as a vector of vectors."
  [tbody-id num-cols]
  (let [table-elem (fn [row col]
                     (try
                       (remove-td (wd/html {:css (format "table>tbody[id=%s]>tr:nth-child(%s)>td:nth-child(%s)"
                                                         tbody-id row col)}))
                       (catch Exception ex
                         :webdriver/table-exhausted)))]
    (->> (for [row (iterate inc 1)
               col (range 1 (+ num-cols 1))]
           (table-elem row col))
         (take-while #(not= % :webdriver/table-exhausted))
         (partition num-cols)
         (map vec)
         vec)))