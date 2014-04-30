(ns kits.map-test
  (:use clojure.test
        conjure.core
        kits.map))

(set! *warn-on-reflection* false)


(deftest test-keywords->underscored-strings
  (is (= {"a_1" 1
          "b_1"  1
          "c-1" 1}
         (keywords->underscored-strings {:a-1 1
                                         :b-1 1
                                         "c-1" 1}))))

(deftest test-keywords->underscored-keywords
  (is (= {:a_1 1
          :b_1 1
          "c-1" 1}
         (keywords->underscored-keywords {:a-1 1
                                          :b-1 1
                                          "c-1" 1}))))

(deftest test-invert-map
  (are [in-map out-map] (invert-map in-map)
       nil           nil
       {}            {}
       {:a 1}        {1 :a}
       {:a {:b 1}}   {{:b 1} :a}

       ;; if the vals aren't unique then funky things will result - beware
       {:a :b :c :b} {:b :c}))

(deftest test-filter-map
  (are [m pred result] (= result (filter-map pred m))
       {}
       (constantly false)
       {}

       nil
       (constantly true)
       nil

       {:a 1 :b 2}
       (fn [k v] (and (= k :a) (= v 1)))
       {:a 1}))

(deftest test-filter-by-key
  (are [m pred result] (= result (filter-by-key pred m))
       {}
       (constantly false)
       {}

       nil
       (constantly true)
       nil

       {:a 1 :b 2}
       (fn [k] (= k :a))
       {:a 1}))

(deftest test-filter-by-val
  (are [m pred result] (= result (filter-by-val pred m))
       {}
       (constantly false)
       {}

       nil
       (constantly true)
       nil

       {:a 1 :b 2}
       (fn [v] (= v 1))
       {:a 1}))

(deftest test-map-over-map
  (are [m f result] (= result (map-over-map f m))
       {}
       (constantly false)
       {}

       nil
       (constantly true)
       nil

       {:a 1 :b 2}
       (fn [k v] [(name k) (inc v)])
       {"a" 2 "b" 3}))

(deftest test-map-keys
  (is (= {:test-:b 1, :test-:a 0} (map-keys (fn [k] (keyword (str "test-" k))) {:a 0 :b 1}))))

(deftest test-map-values
  (is (= {:b 8, :a 7} (map-values #(+ 5 %) {:a 2 :b 3})))
  (is (= {} (map-values #(+ 5 %) {})))
  (is (= nil (map-values #(+ 5 %) nil))))

(deftest test-paths
  (are [m result] (= (paths m) result)
       nil            nil
       {:a 1}         '([:a])
       {:a 1
        :b 2}         '([:a] [:b])
        {:a 1
         :b {:c 2
             :d 3}}    '([:a] [:b :c] [:b :d])))

(deftest test-subpaths
  (are [path sps] (= (subpaths path) sps)
       nil        []
       [:a]       [[:a]]
       [:a :b :c] [[:a] [:a :b] [:a :b :c]]))

(deftest test-subpath?
  (are [root-path path result] (= (subpath? root-path path) result)
       nil nil false
       [:a] [:a] true
       [:a] [:a :b] true
       [:a] [:a :b :c] true
       [:a :b] [:c] false
       [:a :b] [:a] false
       [:a :b] [:a :b] true
       [:a :b] [:a :e :c] false))

(deftest test-select-paths
  (are [m paths result] (= (apply select-paths m paths) result)
       nil                  [[:a :b]] {:a {:b nil}}
       {}                   [[:a :b]] {:a {:b nil}}
       {:a {:b 1}}          [[:a :b]] {:a {:b 1}}

       {:a {:b 1 :c 2 :d 3}
        :x {:y 4 :z 5}
        :n {:m 77}}     [[:a :b] [:a :c] [:x]] {:a {:b 1 :c 2} :x {:y 4 :z 5}}
        ))

(deftest test-contains-path?
  (is (false? (contains-path? nil [:a])))
  (is (false? (contains-path? nil nil)))
  (is (false? (contains-path? nil [])))
  (is (false? (contains-path? {} [:a])))
  (is (false? (contains-path? {} nil)))
  (is (false? (contains-path? {} [])))
  (is (true? (contains-path? {:a 1} [:a])))
  (is (true? (contains-path? {:a {:b 1}} [:a :b]))))

(deftest test-assoc-if-not-present
  (is (= {:a 22} (assoc-if-not-present {:a 22} :a 99)))
  (is (= {:a 99 :b 22} (assoc-if-not-present {:b 22} :a 99)))
  (is (= {:a 99 :b 22 :c 88} (assoc-if-not-present {:b 22} :a 99 :c 88)))
  (is (= {:a 99} (assoc-if-not-present nil :a 99)))
  (is (= {:a 99} (assoc-if-not-present {} :a 99))))

(deftest test-update-in-if-present
  (are [result m] (= result (update-in-if-present m [:a :b] (constantly 99)))

       ;; result       inputted map
       {:a {:b 99}}    {:a {:b 1}}
       {:a 2}          {:a 2}
       {:a {:b 99}}  {:a {:b nil}}
       {:a {:b 99}}  {:a {:b false}}
       nil             nil
       {}              {}))

(deftest test-assoc-in-if-present
  (are [result m] (= result (assoc-in-if-present m [:a :b] 99))

       ;; result       inputted map
       {:a {:b 99}}    {:a {:b 1}}
       {:a 2}          {:a 2}
       {:a {:b 99}}  {:a {:b nil}}
       {:a {:b 99}}  {:a {:b false}}
       nil             nil
       {}              {}))


(deftest test-dissoc-in
  (are [m paths result] (= result (apply dissoc-in m paths))
       {} [[:a :b]] {}
       nil [[:a :b]] nil
       {:a {:b 1 :c 2 :d 3}} [[:a :b] [:a :c]] {:a {:d 3}}))

(deftest test-nested-dissoc
  (are [result x] (= result (nested-dissoc x :a))
       nil nil
       1 1
       {} {}
       #{} #{}

       [1 1] [1 1]
       [{:b 2}] [{:a 1 :b 2}]
       #{{:b 2}} #{{:a 1 :b 2}}

       [{:b [{:b 3}]}] [{:a 1 :b [{:a 1 :b 3}]}]
       ))

(deftest test-deep-merge-with
  (is
   (= {:a {:b {:z 3, :c 3, :d {:z 9, :y 2, :x 1}}, :e 103}, :f 4}

      (deep-merge-with + {:a {:b {:c 1 :d {:x 1 :y 2}} :e 3} :f 4}{:a {:b {:c 2 :d {:z 9} :z 3} :e 100}})))

  (is (= {} (deep-merge-with + {})))
  (is (= nil (deep-merge-with + nil))))

(deftest test-map-difference
  (is (= {:b 1} (map-difference {:a 0 :b 1 :c 0} {:a 0 :b 0 :c 0})))
  (is (= {:b {:c 1}} (map-difference {:a 0 :b {:c 1} :d 0} {:a 0 :b {:c 0} :d 0})))
  (is (= {} (map-difference nil nil)))
  (is (= {} (map-difference {} {})))
  (is (= {} (map-difference {:a 2 :b 2 :c 2} {:a 2 :b 2 :c 2})))

  )

(is (= {:b 1} (map-difference {:a 0 :b 1 :c 0} {:a 0 :b 0 :c 0})))

(deftest test-keys-to-keywords
  (testing "when :underscore-to-hyphens? is true (true by default)"
    (are [expected m] (= expected (keys-to-keywords m :underscore-to-hyphens? true))
         {} {}
         {:a "1"} {"a" "1"}
         {:a {:b {:c {:d "e"}}}} {"a" {"b" {"c" {"d" "e"}}}}
         {:a-1 {:b-2 {:c-d-3 "e"}}} {"a-1" {"b_2" {"c_d-3" "e"}}}))
  (testing "when :underscore-to-hyphens? is true (true by default)"
    (are [expected m] (= expected (keys-to-keywords m :underscore-to-hyphens? false))
         {} {}
         {:a "1"} {"a" "1"}
         {:a {:b {:c {:d "e"}}}} {"a" {"b" {"c" {"d" "e"}}}}
         {:a-1 {:b_2 {:c_d-3 "e"}}} {"a-1" {"b_2" {"c_d-3" "e"}}})))

(deftest test-rmerge
  (is (= {:a 1 :b 2} (rmerge {:b 2} {:a 1})))
  (is (= {:a {:x {:y {:z 3}}} :b 2} (rmerge {:a {:x 1} :b 2} {:a {:x {:y {:z 3}}}})))
  (is (= {:a {:x {:y {:z 3}}} :b 2} (rmerge {:a {:x {:y {:z 1}}} :b 2} {:a {:x {:y {:z 3}}}}))))

(deftest test-submap?
  (is (= true (submap? {} {})))
  (is (= true (submap? nil nil)))
  (is (= true (submap? {:a 1} {:a 1 :b 2})))
  (is (= true (submap? {} {:a 1 :b 2})))
  (is (= true (submap? nil {:a 1 :b 2})))
  (is (= false (submap? {:a 1 :b 2} {:a 1}))))

(deftest test-select-keys-always
  (is (= {:a 1 :b nil :c :default}
         (select-keys-always {:a 1 :b nil} [:a :b :c] :default))))

(deftest test-move-key
  (is (= {:c 1 :b 2}
         (move-key {:a 1 :b 2} :a :c)))
  (is (= {:c 1}
         (move-key {:c 1} :a :b)))
  (is (= {}
         (move-key {} :a :b))))

(deftest test-sorted-zipmap
  (is (= (sorted-map :1 1 :2 2 :3 3 :4 4)
         (sorted-zipmap [:4 :2 :1 :3] [4 2 1 3]))))


(deftest test-assoc-thunk-result-if-not-present
  (instrumenting
   [println]
   (is (= {:a 6} (assoc-thunk-result-if-not-present {} :a (fn [] (do (println 5) 6)))))
   (verify-call-times-for println 1))

  (instrumenting
   [println]
   (is (= {:a 1} (assoc-thunk-result-if-not-present {:a 1} :a (fn [] (do (println 5) 6)))))
   (verify-call-times-for println 0))

  (is (= {:a 1 :b 2} (assoc-thunk-result-if-not-present {} :a (fn [] 1) :b (fn [] 2)))))

(deftest test-rand-select-keys
  (with-redefs [shuffle reverse]
    (is (= {:b 2 :c 3} (rand-select-keys {:a 1 :b 2 :c 3} 2)))
    (is (= {} (rand-select-keys {} 1)))
    (is (= {} (rand-select-keys {} 0)))
    (is (= {} (rand-select-keys {:a 1} 0)))))

(deftest test-copy-key
  (is (= {:b 1 :c 1}
         (copy-key {:b 1} :b :c)))
  (is (= {:a 1}
         (copy-key {:a 1} :b :c))))

(deftest test-let-map
  (is (= {:a 1 :b 1}
         (let-map :a 1
                  :b a)))

  (is (= {"a" 1 "b" 1}
         (let-map "a" 1
                  "b" a))))
