(ns kits.load-driver.users)

(defonce device-tracker-ids (atom #{}))
(defonce bucket-allocations (atom #{}))

(defn random-string [length]
  (apply str (repeatedly length #(rand-nth "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"))))

(defn generate-random-device-tracker-ids [n]
  (dotimes [_ n]
    (swap! device-tracker-ids conj (random-string 10))))

(defn any-device-tracker-id []
  (rand-nth (seq @device-tracker-ids)))

(defn add-bucket-allocations [user-token buckets]
  (doseq [bucket (map :name buckets)]
    (swap! bucket-allocations conj [user-token bucket])))

(defn any-bucket-allocation []
  (rand-nth (seq @bucket-allocations)))

(defn pop-bucket-allocation []
  (let [bucket-allocation (any-bucket-allocation)]
    (swap! bucket-allocations disj bucket-allocation)
    bucket-allocation))

(defn reset-state! []
  (reset! device-tracker-ids #{})
  (reset! bucket-allocations #{}))
