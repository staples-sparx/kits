(ns kits.alist-test
  (:require [kits.alist :as alist]
            [clojure.test :refer :all]))

(deftest test-alists
  (is (= 1
         (alist/get '((a 1)) 'a)))
  (is (= ::default
         (alist/get '((a 1)) 'b ::default)))
  (is (= 1
         (alist/get-in '((a 1)) ['a])))
  (is (= 1
         (alist/get-in '((a (b 2) (c 1))) ['a 'c])))
  (is (= ::default
         (alist/get-in '((a (b 2) (c 1))) ['a 'z] ::default)))
  (is (= ::default
         (alist/get-in '((a (b 2) (c 1))) ['x] ::default))))
