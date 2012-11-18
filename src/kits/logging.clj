(ns kits.logging
  (:require
   [kits.timestamp :as timestamp])
  (:import
   java.io.IOException
   java.util.Date
   java.text.SimpleDateFormat
   [java.util.logging Level Logger Formatter LogRecord Handler]))

(def ^:private ^SimpleDateFormat date-formatter
  (timestamp/simple-date-format "yyyy-MM-dd HH:mm:ss"))

(defn default-formatter [^Formatter formatter ^LogRecord record]
  (println-str (.format date-formatter (Date.))
               (str (.getLevel record))
               (.getName (Thread/currentThread))
               (.formatMessage formatter record)))

(def ^:dynamic *formatter* nil)

(defmacro with-formatter [formatter & body]
  `(binding [*formatter* ~formatter]
     ~@body))

(def formatter
  (proxy [Formatter] []
    (format [^LogRecord record]
      ((or *formatter* default-formatter) ^Formatter this record))))

(def ^:dynamic *default-logger*
  (let [logger (Logger/getAnonymousLogger)]
    (doseq [^Handler handler (.getHandlers (.getParent logger))]
      (.setFormatter handler formatter))
    logger))

(defmacro log [^Level level ^String msg ^Throwable e]
  `(let [l# kits.logging/*default-logger*
         e# ~e
         level# ~level]
     (when (.isLoggable ^Logger l# ^Level level#)
       (try
         (let [msg# (str ~@msg)]
           (if-not e#
             (.log ^Logger l# ^Level level# msg#)
             (.log ^Logger l# ^Level level# msg# ^Throwable e#)))
         (catch IOException ioe#
           ;; Sad but we swallow the exception so that a log message does
           ;; not trigger an unpredictable exception when file system is
           ;; full.
           )))))

(defmacro debug [& msg]
  `(log Level/FINE ~msg nil))

(defmacro info [& msg]
  `(kits.logging/log Level/INFO ~msg nil))

(defmacro warn [& msg]
  `(log Level/WARNING ~msg nil))

(defmacro error [^Throwable exception & msg]
  `(log Level/SEVERE ~msg ~exception))

(defmacro with-logging [& body]
  `(try
     (do ~@body)
     (catch Exception ex#
       (error ex#))))
