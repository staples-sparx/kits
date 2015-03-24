(ns ^{:doc "High throughput logger with fine grained control and performance"}
  kits.logging.log-generator
  (:require
    [kits.queues :as q]
    [cheshire.core :as json])
  (:import
    (java.util Date TimeZone)
    (java.text SimpleDateFormat)
    (java.io StringWriter PrintWriter)))

(set! *warn-on-reflection* true)

(defn stacktrace [^Throwable ex]
  (if (nil? ex)
    ""
    (let [sw (StringWriter. 256)
          _ (doto ex
              (.printStackTrace (PrintWriter. sw)))]
      (.. sw toString trim))))

(defn log-line [level ^String message ^Throwable ex]
  (-> (StringBuilder. 256)
      (.append (Date.))
      (.append \space)
      (.append level)
      (.append \space)
      (.append message)
      (.append \space)
      (.append  (stacktrace ex))
      (.append \newline)
      (.toString)))

(defn json-log-formatter 
  ([context msg-map]
   (let [log-level (get-in msg-map [:log-level] :INFO)
         msg-map (merge context msg-map)]
     (log-line (name log-level) (json/generate-string msg-map) nil)))
  ([default-context context msg-map]
   (json-log-formatter (merge default-context context) msg-map)))

(defn simple-date-format
  ([format-string]
     (simple-date-format format-string "UTC"))
  ([format-string tz-string]
     (doto (SimpleDateFormat. format-string)
       (.setTimeZone
         (TimeZone/getTimeZone tz-string)))))

(defn utc-date-format
  "Return a yyy-MM-dd date format enforcing UTC semantics. Not thread safe!"
  ^SimpleDateFormat
  []
  (simple-date-format "yyyy-MM-dd"))

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

(defn day-at
  "Return a string representing a date as a timestamp since the epoch and using UTC time zone"
  [ts]
  (date->day (Date. (long ts))))

(defn error [^String context ^String message ^Throwable ex]
  (log-line :ERROR context message ex))

(defn warn [^String context ^String message ^Throwable ex]
  (log-line :WARN context message ex))

(defn info [^String context ^String message]
  (log-line :INFO context message nil))

(defn debug [^String context ^String message]
  (log-line :DEBUG context message nil))

(defn trace [^String context ^String message]
  (log-line :TRACE context message nil))

(defn log-file-path-for [config thread-id next-rotate-at]
  (str
   (:root config)
   "/"
   (:filename-prefix config)
   "-"
   next-rotate-at
   "-"
   (day-at next-rotate-at)
   "-"
   thread-id
   ".log"))
