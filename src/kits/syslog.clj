(ns kits.syslog
  "Write wrapper for syslog."
  (:require [kits.syslog.messages :as messages]
            [kits.syslog.udp :as udp]
            [kits.timestamp :as ts])
  (:import kits.syslog.udp.Channel))

;; http://www.springsurprise.com/2010/01/30/enable-remote-logging-on-mac-os-x/
;; http://labs.animoto.com/2008/08/20/ruby-syslog-considered-harmful-or-at-least-undocumented/

(def levels {:debug 7
             :info 6
             :notice 5
             :warn 4
             :error 3
             :critical 2
             :alert 1
             :emergency 0})

(def facilities
  {"kernel messages" 0
   "user-level messages" 1
   "mail system" 2
   "system daemons" 3
   "security/authorization messages" 4
   "messages generated internally by syslogd" 5
   "line printer subsystem" 6
   "network news subsystem" 7
   "UUCP subsystem" 8
   "clock daemon" 9
   "security/authorization messages 2" 10
   "FTP daemon" 11
   "NTP subsystem" 12
   "log audit" 13
   "log alert" 14
   "clock daemon 2" 15
   "local0" 16
   "local1" 17
   "local2" 18
   "local3" 19
   "local4" 20
   "local5" 21
   "local6" 22
   "local7" 23})

(def create-channel udp/create-channel)

(defn log ^Channel [config facility local-name level channel msg]
  (let [chunks (messages/chunks config facility local-name level (ts/now) msg)]
    (reduce
     (fn [channel chunk]
       (udp/write channel chunk))
     channel
     chunks)))

(defn critical
  "Should be corrected immediately, but indicates failure in a primary
   system - fix CRITICAL problems before ALERT - example is loss of
   primary ISP connection"
  ^Channel [config facility local-name channel msg]

  (log config facility local-name (:critical levels) channel msg))

(defn error
  "Non-urgent failures - these should be relayed to developers or
   admins; each item must be resolved within a given time"
  ^Channel [config facility local-name channel msg]

  (log config facility local-name (:error levels) channel msg))

(defn warn
  "Warning messages - not an error, but indication that an error will
   occur if action is not taken, e.g. file system 85% full - each item
   must be resolved within a given time"
  ^Channel [config facility local-name channel msg]

  (log config facility local-name (:warn levels) channel msg))

(defn notice
  "Events that are unusual but not error conditions - might be
  summarized in an email to developers or admins to spot potential
  problems - no immediate action required"
  ^Channel [config facility local-name channel msg]

  (log config facility local-name (:notice levels) channel msg))

(defn info
  "Normal operational messages - may be harvested for reporting,
   measuring throughput, etc - no action required"
  ^Channel  [config facility local-name channel msg]

  (log config facility local-name  (:info levels) channel msg))

(defn debug
  "Info useful to developers for debugging the app, not useful during
   operations"
  ^Channel [config facility local-name channel msg]

  (log config facility local-name (:debug levels) channel msg))


(defn example []
  ;; :local-name "aegis[81357]"
  (let [config {:host "127.0.0.1"
                :port 514
                :retries 10
                :max-msg-length (* 50 1024)
                :split-break-suffix " ..."
                :split-continue-prefix "... "
                :so-timeout-ms 500}
        channel (create-channel config)
        local-name "aegis[7643]"]

    (prn config)
    (critical config (facilities "local0") local-name channel "Hello Syslog Critical")
    (error config (facilities "local0") local-name channel "Hello Syslog Error")
    (warn config (facilities "local0") local-name channel "Hello Warn Syslog 1111234567890123456789012345678901234567890112345678901234567890123456789012345678901234567890")
    (notice config (facilities "local0") local-name channel "Hello Syslog Notice")
    (info config (facilities "local0") local-name channel "Hello Syslog Info")
    (debug config (facilities "local0") local-name channel "Hello Syslog Debug")))

;; (example)
