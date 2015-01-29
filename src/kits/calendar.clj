(ns kits.calendar
  "Convenience function to manipulate time & date on the JVM"
  (:use
    kits.foundation)
  (:import
    java.util.Date
    java.util.Calendar
    java.util.TimeZone
    java.util.GregorianCalendar
    java.text.SimpleDateFormat
    java.text.ParseException))

(def utc-tz (TimeZone/getTimeZone "UTC"))
(def mountain-tz (TimeZone/getTimeZone "MST"))
(def california-tz (TimeZone/getTimeZone "PST"))

(defn simple-date-format
  (^SimpleDateFormat [format-string]
     (simple-date-format format-string "UTC"))
  (^SimpleDateFormat [format-string tz-string]
     (doto (SimpleDateFormat. format-string)
       (.setTimeZone
         (TimeZone/getTimeZone tz-string)))))

(defn utc-date-format
  "Return a yyy-MM-dd date format enforcing UTC semantics. Not thread safe!"
  ^SimpleDateFormat
  []
  (simple-date-format "yyyy-MM-dd"))

(defn utc-datetime-format
  "Return a 'yyy-MM-dd HH:mm' date format enforcing UTC semantics. Not thread safe!"
  ^SimpleDateFormat
  []
  (simple-date-format "yyyy-MM-dd HH:mm"))

(defn full-datetime-format
  "Return a 'yyy-MM-dd HH:mm' date format enforcing UTC semantics. Not thread safe!"
  ^SimpleDateFormat
  ([]
     (full-datetime-format "UTC"))
  ([tz-string]
     (simple-date-format "yyyy-MM-dd HH:mm z" tz-string)))

(defn str->ts
  ([^String str]
     (str->ts str (full-datetime-format)))
  ([^String str ^SimpleDateFormat format]
     (try
       (.getTime
         (.parse format str))
       (catch ParseException e
         nil))))

(defn date-to-string
  "Converts date to yyyy-MM-dd format"
  ([^Date d]
     (date-to-string d (utc-date-format)))
  ([^Date d ^SimpleDateFormat formatter]
     (when d
       (.format formatter d))))

(defn date->day
  "Return a string representing a date in YYYY-MM-dd format and using UTC time zone"
  [^Date day]
  (date-to-string day (utc-date-format)))

(defn ts->daytime
  "Return a string representing a date in YYYY-MM-dd format and using UTC time zone"
  [ts]
  (date-to-string (Date. (long ts)) (utc-datetime-format)))

(defn ts->california-daytime
  "Return a string representing a date in YYYY-MM-dd format and using PST time zone"
  [ts]
  (date-to-string
    (Date. (long ts))
    (full-datetime-format "PST")))

(defn day-at
  "Return a string representing a date as a timestamp since the epoch and using UTC time zone"
  [ts]
  (date->day (Date. (long ts))))

(defn daytime-at
  "Return a string representing a date as a timestamp since the epoch and using UTC time zone"
  [ts]
  (date-to-string (Date. (long ts))  (utc-datetime-format)))

(defn utc-cal-at
  "Returns a UTC Java calendar set at a specific point-of-time (timestamp in ms)"
  [ts]
  (doto (Calendar/getInstance)
    (.setTimeZone utc-tz)
    (.setTimeInMillis ts)))

(defn mountain-cal-at
  "Returns a US Mountain Time Java calendar set at a specific point-of-time (timestamp in ms)"
  [ts]
  (doto (Calendar/getInstance)
    (.setTimeZone mountain-tz)
    (.setTimeInMillis ts)))

(defn ts->end-of-day-ts
  "Return a timestamp since the epoch representing start of a day for an arbitrary ms timestamp and using UTC time zone"
  [ts]
  (.getTimeInMillis
    (doto ^Calendar (utc-cal-at ts)
      (.set Calendar/HOUR_OF_DAY 23)
      (.set Calendar/MINUTE 59)
      (.set Calendar/SECOND 59)
      (.set Calendar/MILLISECOND 999))))

(defn round-up-ts
  "Return a timestamp rounded up to the next 'n' minutes"
  [ts n-minutes]
  (let [c ^Calendar(utc-cal-at ts)
        min (.get c Calendar/MINUTE)
        n (Math/floor (/ min n-minutes))
        rounded-down (.getTimeInMillis
                       (doto ^Calendar c
                         (.set Calendar/MINUTE (* n n-minutes))))]
    (_+ rounded-down (_* n-minutes 60000))))
