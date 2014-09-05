(ns kits.timestamp
  "Functions for creating and formatting timestamps (Longs)"
  (:require [clojure.string :as str])
  (:import (java.sql Timestamp)
           (java.text SimpleDateFormat)
           (java.util Calendar Date GregorianCalendar TimeZone)))

(set! *warn-on-reflection* true)


;;; formats

(def ^:const yyyy-mm-dd "yyyy-MM-dd")

(defn yyyy-mm-dd? [x]
  (and (string? x)
       (boolean (re-matches #"\d{4}-\d{2}-\d{2}" x))))

(def ^:const yyyy-mm-dd-hh-mm "yyyy-MM-dd HH:mm")

(defn yyyy-mm-dd-hh-mm? [x]
  (and (string? x)
       (boolean (re-matches #"\d{4}-\d{2}-\d{2} \d{2}:\d{2}" x))))

(def ^:const yyyy-mm-dd-hh-mm-ss "yyyy-MM-dd HH:mm:ss")

(defn yyyy-mm-dd-hh-mm-ss? [x]
  (and (string? x)
       (boolean (re-matches #"\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2}" x))))

(def ^:const american-date-format "MM/dd/yyyy")

;;;

(def ^TimeZone utc-tz (TimeZone/getTimeZone "UTC"))

(defn ^:dynamic now []
  (System/currentTimeMillis))

(defn now-s []
  (Math/round (/ (System/currentTimeMillis) 1000.0)))

(defn ^SimpleDateFormat simple-date-format
  ([format-string]
     (simple-date-format format-string utc-tz))
  ([format-string tz-or-tz-str]
     (let [^TimeZone tz (if (instance? TimeZone tz-or-tz-str)
                          tz-or-tz-str
                          (TimeZone/getTimeZone tz-or-tz-str))]
       (doto (SimpleDateFormat. format-string)
         (.setTimeZone tz)))))

(defn ^Long ->timestamp
  ([s format-string tz-or-tz-str]
     (-> (simple-date-format format-string tz-or-tz-str)
         (.parse s)
         .getTime))
  ([s format-string]
     (->timestamp s format-string utc-tz))
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
     (->str x date-format utc-tz))
  ([x date-format tz-or-tz-str]
     (.format
      (simple-date-format date-format tz-or-tz-str)
      (Date. (long (->timestamp x))))))

(defn ->date-str [x]
  (.format
   (simple-date-format yyyy-mm-dd)
   (Date. (long (->timestamp x)))))

;;;

(def ^:const ordered-units
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
  (let [cal (doto ^Calendar (GregorianCalendar. utc-tz)
                  (.setTime (Date. (long timestamp))))
        units-to-drop (take-while #(not= unit %) ordered-units)]
    (doseq [{:keys [type starts-at]} (map unit->calendar-unit units-to-drop)]
      (.set cal type starts-at))
    (.getTimeInMillis cal)))

(defn add [timestamp unit val]
  (assert-valid-unit unit)
  (let [cal (doto ^Calendar (GregorianCalendar. utc-tz)
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

(defn ->timestamp-at-day-start
  ([]
     (->timestamp-at-day-start (now)))
  ([t]
     (-> t ->timestamp (truncate :day))))

(defn timestamp-at-day-end 
  ([]
     (timestamp-at-day-end (now)))
  ([ts]
     (-> ts (add :day 1) (decrement :second))))

(defn ->timestamp-at-day-end 
  ([]
     (->timestamp-at-day-end (now)))
  ([t]
     (-> t ->timestamp timestamp-at-day-end)))


;;; Ranges

(def ^:private legal-interval-set 
  #{"none" "second" "minute" "hour" "day" "week" "month" "year"})

(defmulti ^:private timestamp-ranges-internal
  (fn [_ _ interval]
    (if (contains? #{:second :minute :hour :day :week :year} (keyword interval))
      :standard-interval
      (keyword interval))))

(defn timestamp-ranges [start-date end-date interval]
  (if (contains? legal-interval-set (name interval))
    (timestamp-ranges-internal start-date end-date interval)
    (if-let [matching-interval-prefix (->> legal-interval-set
                                           (filter #(.startsWith (name interval) %))
                                           first)]
      (let [[interval' n] (str/split (name interval) #"-")]
        (->> (timestamp-ranges-internal start-date end-date (keyword interval'))
             (partition (Long/parseLong n))
             (map #(vector (ffirst %) (last (last %))))))
      (throw (IllegalArgumentException. (format "Can't use supplied interval '%s'"
                                                interval))))))

(defmethod timestamp-ranges-internal :none [start-date end-date interval]
  [[(->timestamp start-date) (->timestamp end-date)]])

(defmethod timestamp-ranges-internal
  :standard-interval
  [start-date end-date interval]
  (let [interval (keyword interval)
        end (increment (->timestamp end-date) interval)
        ranges (->> (->timestamp start-date)
                    (iterate #(increment % interval))
                    (take-while #(<= % end))
                    (partition 2 1))]
    (map (fn [[from to]]
           [from (decrement to :milli)])
         ranges)))

(defmethod timestamp-ranges-internal :month [start-date end-date interval]
  (let [interval (keyword interval)
        start-ts (->timestamp start-date)
        end-ts (->timestamp end-date)
        month-decrementor (fn [[from to]]
                            [from (-> to
                                      (truncate :month)
                                      (decrement :milli))])
        ranges (->> (truncate start-ts :month)
                    (iterate #(increment % interval))
                    (take-while #(<= % (increment end-ts interval)))
                    (partition 2 1))
        first-month-pair (first ranges)
        last-month-pair (last ranges)
        month-pairs-middle (butlast (rest ranges))
        fix-first (fn [[_ end]] 
                    [start-ts end])
        fix-last  (fn [[start _]]
                    [start (-> end-ts (add :day 1) (decrement :milli))])
        mid-month-ranges (map month-decrementor month-pairs-middle)
        flat-month-ranges (flatten [(fix-first (month-decrementor first-month-pair))
                                    mid-month-ranges
                                    (fix-last last-month-pair)])]
    (partition 2 flat-month-ranges)))

;; SQL helpers

(defn ->sql-time [timestamp]
  (->str timestamp "yyyy-MM-dd HH:mm:ss.SSS"))

(defn ->Timestamp
  "Creating a UTC string and parsing it adjusts the timezone
   properly. (Timestamp. millis) alters the millis into the local timezone"
  [ts]
  (Timestamp/valueOf (->sql-time ts)))
