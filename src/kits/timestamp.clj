(ns kits.timestamp
  (:import
   java.text.SimpleDateFormat
   [java.util Date Calendar TimeZone GregorianCalendar]))


;;; formats

(def ^:constant yyyy-mm-dd "yyyy-MM-dd")

(defn yyyy-mm-dd? [x]
  (and (string? x)
       (boolean (re-matches #"\d{4}-\d{2}-\d{2}" x))))

(def ^:constant yyyy-mm-dd-hh-mm "yyyy-MM-dd HH:mm")

(defn yyyy-mm-dd-hh-mm? [x]
  (and (string? x)
       (boolean (re-matches #"\d{4}-\d{2}-\d{2} \d{2}:\d{2}" x))))

(def ^:constant yyyy-mm-dd-hh-mm-ss "yyyy-MM-dd HH:mm:ss")

(defn yyyy-mm-dd-hh-mm-ss? [x]
  (and (string? x)
       (boolean (re-matches #"\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2}" x))))

;;;

(defn now []
  (System/currentTimeMillis))

(defn ^SimpleDateFormat simple-date-format
  ([format-string]
     (simple-date-format format-string "UTC"))
  ([format-string tz-string]
     (doto (SimpleDateFormat. format-string)
       (.setTimeZone (TimeZone/getTimeZone tz-string)))))

(defn ^Long ->timestamp
  ([s format-string timezone-str]
     (-> (simple-date-format format-string timezone-str)
         (.parse s)
         .getTime))
  ([s format-string]
     (->timestamp s format-string "UTC"))
  ([x]
     (cond
      (integer? x)
      (long x)

      (yyyy-mm-dd? x)
      (->timestamp x yyyy-mm-dd)

      (yyyy-mm-dd-hh-mm? x)
      (->timestamp x yyyy-mm-dd-hh-mm)

      (yyyy-mm-dd-hh-mm-ss? x)
      (->timestamp x yyyy-mm-dd-hh-mm-ss)

      (and (string? x) (re-matches #"\d+" x))
      (Long/parseLong x)

      :else
      (throw (IllegalArgumentException. (str "Don't know how to parse " (pr-str x)))))))

(defn ->str
  ([x]
     (->str x yyyy-mm-dd-hh-mm-ss))
  ([x date-format]
     (.format
      (simple-date-format date-format)
      (Date. (long (->timestamp x))))))

(defn ->date-str [x]
  (.format
   (simple-date-format yyyy-mm-dd)
   (Date. (long (->timestamp x)))))

;;;

(def ordered-units
  [:milli
   :second
   :minute
   :hour
   :day
   :month
   :year])

(def unit->calendar-unit
  {:milli  {:type Calendar/MILLISECOND  :starts-at 0}
   :second {:type Calendar/SECOND       :starts-at 0}
   :minute {:type Calendar/MINUTE       :starts-at 0}
   :hour   {:type Calendar/HOUR_OF_DAY  :starts-at 0}
   :day    {:type Calendar/DAY_OF_MONTH :starts-at 1}
   :month  {:type Calendar/MONTH        :starts-at 0}
   :week   {:type Calendar/WEEK_OF_YEAR :starts-at 1}
   :year   {:type Calendar/YEAR         :starts-at 0}})

(defn assert-valid-unit [unit]
  (when-not (contains? unit->calendar-unit unit)
    (throw (IllegalArgumentException. (str unit " not one of " ordered-units)))))

(defn truncate [timestamp unit]
  (assert-valid-unit unit)
  (let [cal (doto ^Calendar (GregorianCalendar. ^TimeZone (TimeZone/getTimeZone "UTC"))
              (.setTime (Date. (long timestamp))))
        units-to-drop (take-while #(not= unit %) ordered-units)]
    (doseq [{:keys [type starts-at]} (map unit->calendar-unit units-to-drop)]
      (.set cal type starts-at))
    (.getTimeInMillis cal)))

(defn add [timestamp unit val]
  (assert-valid-unit unit)
  (let [cal (doto ^Calendar (GregorianCalendar. ^TimeZone (TimeZone/getTimeZone "UTC"))
              (.setTime (Date. (long timestamp)))
              (.add (:type (unit->calendar-unit unit)) val))]
    (-> cal .getTime .getTime)))

(defn subtract [timestamp unit val]
  (add timestamp unit (* -1 val)))

(defn increment [timestamp unit]
  (add timestamp unit 1))

(defn decrement [timestamp unit]
  (add timestamp unit -1))

;;;

(defn overlap? [[a0 a1] [b0 b1]]
  (and
    (< a0 b1)
    (> a1 b0)))

(defn ->timestamp-at-day-start [t]
  (-> t ->timestamp (truncate :day)))

(defn timestamp-at-day-end [ts]
  (-> ts (add :day 1) (decrement :second)))

(defn ->timestamp-at-day-end [t]
  (-> t ->timestamp timestamp-at-day-end))

(defmulti timestamp-ranges
  (fn [_ _ interval]
    (if (#{:minute :hour :day :week :year} (keyword interval))
      :standard-interval
      (keyword interval))))

(defmethod timestamp-ranges :standard-interval [start-date end-date interval]
  (let [interval (keyword interval)
        start (->timestamp start-date)
        end-ts (->timestamp end-date)
        end (increment end-ts interval)
        stdrd-decrementor (fn [[from to]]
                            [from (decrement to :second)])
        ranges (->> start
                    (iterate #(increment % interval))
                    (take-while #(<= % end))
                    (partition 2 1))]
    (map stdrd-decrementor ranges)) )

(defmethod timestamp-ranges :month [start-date end-date interval]
  (let [interval (keyword interval)
        start-ts (->timestamp start-date)
        end-ts (->timestamp end-date)
        month-decrementor (fn [[from to]]
                            [from (-> to
                                      (truncate :month)
                                      (decrement :second))])
        ranges (->> (truncate start-ts :month)
                    (iterate #(increment % interval))
                    (take-while #(<= % (increment end-ts interval)))
                    (partition 2 1))
        first-month-pair (first ranges)
        last-month-pair (last ranges)
        month-pairs-middle (butlast (rest ranges))
        fix-first (fn [[f _]] [start-ts _])
        fix-last  (fn [[_ l]] [_ (timestamp-at-day-end end-ts)])
        mid-month-ranges (flatten (map month-decrementor month-pairs-middle))
        flat-month-ranges (flatten [(fix-first (month-decrementor first-month-pair))
                                    mid-month-ranges
                                    (fix-last last-month-pair)])]
    (partition 2 flat-month-ranges)))
