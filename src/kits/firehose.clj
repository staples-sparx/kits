(ns kits.firehose
  "Infinite lazy seqs that can be globally turned on/off.
   Originally used for memory profiling."
  (:refer-clojure :exclude [cycle repeat]))

(defonce off? (atom false))

(defonce sleep-millis (atom 100))

(defn repeat [x]
  (lazy-seq
   (if @off?
     nil
     (do
       (when (> @sleep-millis 0)
         (Thread/sleep @sleep-millis))
       (cons x (repeat x))))))

(defn cycle [coll]
  (lazy-seq
   (when-let [s (if @off?
                  nil
                  (seq coll))]
     (do
       (when (> @sleep-millis 0)
         (Thread/sleep @sleep-millis))
       (concat s (cycle s))))))

(defn on! []
  (reset! off? false))

(defn off! []
  (reset! off? true))
