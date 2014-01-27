(ns kits.syslog.messages
  (:require [kits.timestamp :as ts])
  (:import java.text.SimpleDateFormat
           java.util.Date))

(def date-format-pattern "MMM dd HH:mm:ss");

(def ^ThreadLocal date-format-holder (ThreadLocal.))

(defn ^SimpleDateFormat thread-local-date-format []
  (or (.get date-format-holder)
      (let [df (ts/simple-date-format date-format-pattern)]
        (.set date-format-holder df)
        df)))

(defn append-date-prefix [^StringBuffer sb ts]
  (let [from (.length sb)
        pos (+ from 4)
        df (thread-local-date-format)]
    (.format df
             (Date. ^long ts)
             sb
             (java.text.FieldPosition. 0))
    (when (= \0 (.charAt sb pos))
      (.setCharAt sb pos \ )))) ;; RFC 3164 requires leading space for days 1-9

(defn append-header [^StringBuffer sb facility level local-name ts]
  (doto sb
    (.append "<")
    (.append (bit-or (* 8 facility) level))
    (.append ">")
    (append-date-prefix ts)
    (.append " ")
    (.append local-name)))

(defn chunks [config facility local-name level ts msg]
  (let [msg (str " " msg)
        max (:max-msg-length config)
        sb (StringBuffer. 50000)
        _ (append-header sb facility level local-name ts)
        header-length (.length sb)
        _ (.append sb msg)
        l (.length sb)]
    (if (<= l max)
      [(str sb)]
      (let [prefix (or ^String  (:split-continue-prefix config) "")
            prefix-length (.length prefix)
            suffix (or ^String (:split-break-suffix config) "")
            suffix-length (.length suffix)
            until (- max suffix-length)
            pos (- until header-length)
            _ (.setLength sb until)
            _ (.append sb suffix)
            first-chunk (str sb)
            msg-length (.length msg)
            _ (.setLength sb header-length)
            _ (.append sb prefix)
            header-length (.length sb)
            msg-capacity (- max header-length)]
        (loop [c [first-chunk]
               pos pos]
          (.setLength sb header-length)
          (cond
           (>= pos msg-length)                  ;; Done
           c

           (<= (- msg-length pos) msg-capacity) ;; Last
           (do (.append sb (subs msg pos))
               (recur
                (conj c (str sb))
                msg-length))

           :otherwise
           (let [capacity (- msg-capacity suffix-length)]
             (.append sb (subs msg pos (+ pos capacity)))
             (.append sb (String. suffix))
             (recur
              (conj c (str sb))
              (+ pos capacity)))))))))
