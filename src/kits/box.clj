(ns kits.box
  "It's a box. You can deref it.")

(defn box [val]
  (reify clojure.lang.IDeref
    (deref [_] val)))
