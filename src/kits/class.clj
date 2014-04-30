(ns kits.class
  "Class and instance related utilities" )

(set! *warn-on-reflection* true)

(defn base-array?
  "Test if specified array is of a base-type (long/double etc.)"
  [a]
  (and (or a false) (.isArray ^Class (class a))))

(defn safe-cast
  "Determines if given 'object' can be cast to an instance of the given 'class'.
   Returns the casted instance if successfull, nil otherwise"
  [class object]
  (try (cast class object)
       (catch Exception e nil)))
