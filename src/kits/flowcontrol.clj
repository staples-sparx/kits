(ns ^{:doc "New flow control constructs"}
  kits.flowcontrol)

(set! *warn-on-reflection* true)


(defmacro if-lets
  ([bindings expr1]
     `(if-lets ~bindings
               ~expr1
               nil))
  ([bindings expr1 expr2]
     (if (= 2 (count bindings))
       `(if-let ~bindings
          ~expr1
          ~expr2)
       `(if-let ~(vec (take 2 bindings))
          (if-lets ~(vec (drop 2 bindings))
                   ~expr1
                   ~expr2)
          ~expr2))))

(defmacro when-lets [bindings & body]
  `(if-lets ~bindings
            (do ~@body)
            nil))
