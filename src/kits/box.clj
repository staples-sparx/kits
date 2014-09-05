(ns kits.box
  "It's a box. You can deref it.")

(defn box [val]
  (reify
    clojure.lang.IDeref
    (deref [_]
      val)
    clojure.lang.IBlockingDeref
    (deref [_ _timeout-ms _timeout-val]
      val)
    clojure.lang.IPending
    (isRealized [_]
      true)))
