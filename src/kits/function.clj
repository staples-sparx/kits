(ns ^{:doc "Higher order functions for dealing with other functions"}
  kits.function)

(set! *warn-on-reflection* true)


(defn flip
  "Given a 2-arg function, creates a new function that
   accepts 2 args in reverse order."
  [f]
  (fn [a b]
    (f b a)))
