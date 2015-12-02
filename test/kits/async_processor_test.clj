(ns kits.async-processor-test
  (:require [clojure.test :refer :all]
            [kits.async-processor :as p]))

(defn- wait-for
  "Invoke predicate every interval (default 10) seconds until it returns true,
  or timeout (default 150) seconds have elapsed. E.g.:

  (wait-for #(< (rand) 0.2) :interval 1 :timeout 10)

  Returns nil if the timeout elapses before the predicate becomes true, otherwise
  the value of the predicate on its last evaluation."
  [predicate & {:keys [interval timeout sleep]
                :or {interval 1
                     timeout 5
                     sleep 1}}]
  (Thread/sleep (* sleep 1000))
  (let [end-time (+ (System/currentTimeMillis) (* timeout 1000))]
    (loop []
      (if-let [result (predicate)]
        result
        (do
          (Thread/sleep (* interval 1000))
          (if (< (System/currentTimeMillis) end-time)
            (recur)))))))

(deftest can-create-and-start-thread-pool
  (testing "We can create queue, thread-pool and process loop"
    (let [aval (atom 0)
          process-name "test"]
      (p/create-and-start-thread-pool
       {:process-fn #(reset! aval %)
        :exception-handler (constantly nil)
        :name process-name
        :queue-count 10
        :thread-count 1
        :sleep-interval 10
        :info-logger println
        :exception-logger println})
      (p/enqueue process-name 1)
      (wait-for #(p/q-empty? process-name))
      (is (= 1 @aval))))

  (testing "We can handle exception with retry times"
    (let [error-count (atom 0)
          final-count (atom 0)
          process-name "test-exception"]
      (p/create-and-start-thread-pool
       {:process-fn #(/ % 0)
        :exception-handler (fn [& _]
                             (swap! error-count inc) (throw (Exception.)))
        :final-handler (fn [& _] (swap! final-count inc))
        :name process-name
        :queue-count 10
        :thread-count 1
        :try-times 3
        :info-logger println
        :exception-logger println})
      (p/enqueue process-name 1)
      (wait-for #(p/q-empty? process-name))
      (is (= 3 @error-count))
      (is (= 1 @final-count))))

  (testing "We can create queue, thread-pool and process loop with batching"
    (let [aval (atom nil)
          process-name "test-batching"]
      (p/create-and-start-thread-pool
       {:process-fn #(reset! aval %)
        :exception-handler (constantly nil)
        :name process-name
        :min-batch-count 3
        :queue-count 6
        :thread-count 1
        :sleep-interval 10
        :info-logger println
        :exception-logger println})
      (p/enqueue process-name :item-1)
      (wait-for #(not (p/q-empty? process-name)))
      (is (= nil @aval))
      (p/enqueue process-name :item-2)
      (p/enqueue process-name :item-3)
      (wait-for #(p/q-empty? process-name))
      (is (= #{:item-1 :item-2 :item-3} (set @aval))))))

(defn- start-count-workers
  [process-name
   & {:keys [queue-count thread-count process-fn]
      :or {queue-count 100 thread-count 1}}]
  (let [counts (atom {:success 0
                      :retry 0
                      :failure 0})]
    (p/create-and-start-thread-pool
     {:process-fn (or process-fn
                      (fn [& _] (swap! counts update-in [:success] inc)))
      :exception-handler (fn [& _] (swap! counts update-in [:retries] inc))
      :final-handler (fn [& _] (swap! counts update-in [:failures] inc))
      :name process-name
      :queue-count queue-count
      :thread-count thread-count
      :try-times 3
      :info-logger println
        :exception-logger println})
    counts))

(deftest q-percent-free
  (testing "An empty queue is 100% free"
    (let [process-name "empty"]
      (start-count-workers process-name)
      (is (= 100 (p/q-percent-free process-name)))))

  (testing "A full queue is 0% free"
    (let [process-name "full"]
      (start-count-workers
       process-name
       :queue-count 10
       :process-fn (fn [& _] (while true (Thread/sleep (* 10 1000)))))
      (dotimes [_ 10]
        (p/enqueue process-name true))
      (Thread/sleep 1000)
      (p/enqueue process-name true)
      (is (= 0 (p/q-percent-free process-name)))))

  (testing "A full queue can become empty"
    (let [process-name "full-to-empty"
          keep-going? (atom true)]
      (start-count-workers
       process-name
       :queue-count 10
       :process-fn (fn [& _] (while @keep-going? (Thread/sleep 100))))
      (dotimes [_ 10]
        (p/enqueue process-name true))
      (is (> 50 (p/q-percent-free process-name)))
      (reset! keep-going? false)
      (wait-for #(p/q-empty? process-name) :interval 0.1)
      (is (= 100 (p/q-percent-free process-name))))))

(deftest ^:api-benchmark run-process-through-the-paces
  (testing "We can handle things under light load"
    (let [process-name "light-load"
          counts (start-count-workers process-name)
          enqueue-count 10]
      (dotimes [_ enqueue-count]
        (p/enqueue process-name true))
      (wait-for #(p/q-empty? process-name))
      (is (= {:success enqueue-count
              :retry 0
              :failure 0}))))

  (testing "We can handle things under normal load"
    (let [process-name "normal-load"
          counts (start-count-workers process-name)
          enqueue-count 20]
      (dotimes [_ enqueue-count]
        (p/enqueue process-name true))
      (wait-for #(p/q-empty? process-name))
      (is (= {:success enqueue-count
              :retry 0
              :failure 0}
             @counts))))

  (testing "We can handle things under heavy load"
    (let [process-name "heavy-load"
          counts (start-count-workers process-name)]
      (dotimes [_ 100]
        (p/enqueue process-name true))
      (wait-for #(p/q-empty? process-name))
      (is (= {:success 100
              :retry 0
              :failure 0}
             @counts))))

  (testing "With too many requests we'll drop some of them"
    (let [process-name "overfill"
          counts (start-count-workers process-name)
          enqueue-count 1000]
      (dotimes [_ enqueue-count]
        (p/enqueue process-name true))
      (wait-for #(p/q-empty? process-name))
      (is (> enqueue-count (:success @counts)))
      (is (= {:retry 0 :failure 0}
             (dissoc @counts :success)))))

  (testing "A really really big set of workers"
    (let [process-name "big"
          counts (start-count-workers
                  process-name :queue-count 1000 :thread-count 3)
          enqueue (fn [times]
                    (dotimes [_ times]
                      (p/enqueue process-name true)))
          first-count 500
          second-count 500
          third-count 1000]
      (enqueue first-count)
      (Thread/sleep 100)
      (enqueue second-count)
      (wait-for #(p/q-empty? process-name) :sleep 0.1)
      (enqueue third-count)
      (wait-for #(p/q-empty? process-name) :sleep 0.1)
      (is (= {:success (+ first-count second-count third-count)
              :retry 0
              :failure 0}
             @counts)))))
