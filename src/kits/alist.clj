(ns ^{:doc "Association lists, also known as a-lists.
            http://www.gnu.org/software/emacs/manual/html_node/elisp/Association-Lists.html"}
  kits.alist
  (:refer-clojure :exclude [get get-in]))

(defn- get-with [f alist k default]
  (if-let [element (first (filter #(= k (first %)) alist))]
    (f element)
    default))

(defn get
  "Takes an alist, like '((cost 100) (criteria true)) and returns the value for the
   specified key, for example k='cost returns 100"
  ([alist k]
     (get alist k nil))
  ([alist k default]
     (get-with second alist k default)))

(defn get-in
  "Ex. (get-in '((a (b 1) (c 2))) ['a 'c]) => 2"
  ([alist ks]
     (get-in alist ks nil))
  ([alist [k & more-ks] default]
     (if more-ks
       (get-in (get-with rest alist k default) more-ks default)
       (get-with second alist k default))))
