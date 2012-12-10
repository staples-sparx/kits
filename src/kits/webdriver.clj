;; clj-webdriver does not support Clojure 1.2, so these utils only
;; exist when the Clojure version is past 1.2
(in-ns 'kits.homeless)

(when-after-clojure-1-3
  (ns kits.webdriver
   (:require [clj-webdriver.taxi :as wd]))


 (defn- remove-td [s]
   (->> (.trim ^String s)
        (take (- (count s) 5))
        (drop 4)
        (apply str)))

 (defn extract-html-table
   "Extracts a table from a webpage, representing it as a vector of vectors."
   [tbody-id num-cols & {:keys [hidden-table?]}]
   (let [table-elem (fn [row col]
                      (try
                        (let [selector {:css (format "table>tbody[id=%s]>tr:nth-child(%s)>td:nth-child(%s)" tbody-id row col)}]
                          (if hidden-table? 
                            ;; Webdriver needs some trickery to
                            ;; read hidden HTML. This is not perfect
                            ;; it won't work if there are attributes,
                            ;; for instance, improvements are welcome
                            (remove-td (wd/html selector))
                            (wd/text selector)))
                        (catch Exception ex
                          :webdriver/table-exhausted)))]
     (->> (for [row (iterate inc 1)
                col (range 1 (+ num-cols 1))]
            (table-elem row col))
          (take-while #(not= % :webdriver/table-exhausted))
          (partition num-cols)
          (map vec)
          vec)))

)