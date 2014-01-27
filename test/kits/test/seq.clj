(ns kits.test.seq
  (:use clojure.test
        kits.seq))


(deftest test-any?
  (is (= true (any? #{'a 'b} ['a])))
  (is (= false (any? #{'a 'b} ['c]))))

(deftest test-distinct-by
  (is (= [{:a 1 :b 3} {:a 2 :b 2}]
         (distinct-by :a [{:a 1 :b 3} {:a 2 :b 2} {:a 1 :b 2}])))

  (is (= [{:a 1 :b 3} {:a 2 :b 2} {:a 1 :b 2}]
         (distinct-by :a 2 [{:a 1 :b 3} {:a 2 :b 2} {:a 1 :b 2} {:a 1 :b 7}]))))

(deftest test-ensure-sequential
  (are [result x] (= result (ensure-sequential x))
       nil     nil
       [{}]      {}
       [{:a 1}]  {:a 1}
       [1]       1
       [1 2 3]   [1 2 3]))

(deftest test-indexed
  (is (= '([0 a] [1 b] [2 c] [3 d]) (indexed '(a b c d))))
  (is (= '() (indexed 'nil)))
  (is (= '() (indexed '()))))

(deftest test-only
  (testing "when not 1 item"
    (is (thrown-with-msg? RuntimeException #"should have precisely one item, but had 0" (only [])))
    (is (= 1 (only [1])))
    (is (thrown-with-msg? RuntimeException #"should have precisely one item, but had at least 2" (only [1 2])))
    (is (thrown-with-msg? RuntimeException #"should have precisely one item, but had at least 2" (only (repeat 5))))))

(deftest test-segregate
  (is (= [nil nil] (segregate string? nil)))

  (is (= [["a" "b"] [1 2]] (segregate string? [1 "a" "b" 2])))
  (is (= [[] []] (segregate string? [])))

  (is (= ['("b" "a") '(2 1)] (segregate string? '(1 "a" "b" 2))))
  (is (= ['() '()] (segregate string? '())))

  (is (= [#{"a" "b"} #{1 2}] (segregate string? #{1 "a" "b" 2})))
  (is (= [#{} #{}] (segregate string? #{}))))

(deftest test-seq-to-map
  (is (= {:a 2, :b 4, :c 5} (seq-to-map '([:a 2] [:b 4] [:c 5]))))
  (is (nil? (seq-to-map nil)))
  (is (nil? (seq-to-map '()))))

(deftest test-zip
  (are [lists result] (= (zip lists) result)
       nil []
       []  []
       [[:a 1 \x]]             [[:a] [1] [\x]]
       [[:a] [1] [\x]]         [[:a 1 \x]] ;; reversible!
       [[:a 1 \x] [:b 2 \y]]   [[:a :b] [1 2] [\x \y]]
       [[:a :b] [1 2] [\x \y]] [[:a 1 \x] [:b 2 \y]])) ;; reversible!

(deftest test-count-exact-occurences
  (is (= 1 (count-exact-occurences ["bob" "Bob" "bob bob bob"] ["bob"])))
  (is (= 2 (count-exact-occurences ["bob" "Bob" "bob bob bob"] ["bob" "Bob"]))))

(deftest test-count-matching-occurences
  (is (= 3 (count-matching-occurences ["bob" "Bob" "bob bob bob"] ["bob"]))))

(deftest test-map-nth
  (is (= [1 2 1 2] (map-nth inc 2 [1 1 1 1]))))
