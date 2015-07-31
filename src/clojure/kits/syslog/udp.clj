(ns ^{:doc "Provides a writer interface to UDP by wrapping java.net classes."}
  kits.syslog.udp
  (:import
    java.io.IOException
    java.net.DatagramPacket
    java.net.DatagramSocket
    java.net.InetAddress
    java.nio.charset.Charset))

(set! *warn-on-reflection* true)

(def ^Charset utf-8-charset (Charset/forName "UTF-8"))

(defn create-socket [so-timeout-ms]
  (doto (DatagramSocket.)
    (.setSoTimeout so-timeout-ms)))

(defrecord Channel [config ^DatagramSocket socket ^InetAddress addr ^long port])

(defn create-channel ^Channel [config]
  (Channel.
   config
   (create-socket
    (:so-timeout-ms config))
   (InetAddress/getByName (:host config))
   (long (:port config))))

(defn recreate-channel ^Channel [channel]
  (create-channel (:config channel)))

(defn send-packet [^DatagramSocket socket ^DatagramPacket packet]
  (try
    (.send ^DatagramSocket socket packet)
    nil
    (catch IOException e
      e)))

(defn write
  "Write a chunk on the UDP channel. For automatic recovery purposes
  the provided channel is recreated on IOException. Caller should use
  the returned channel for the next call to benefit from it."
  ^Channel [^Channel channel ^String chunk]

  (let [payload (.getBytes chunk utf-8-charset)
        retries (get-in channel [:config :retries])
        l (alength payload)]
    (loop [channel channel
           remaining retries]
      (let [packet (DatagramPacket.
                    payload
                    l
                    ^InetAddress (:addr channel)
                    ^long (:port channel))
            error (send-packet (:socket channel) packet)]
        (if error
          (if (pos? remaining)
            (recur
             (recreate-channel)
             (dec remaining))
            (throw (RuntimeException. ^Throwable error)))
          channel)))))
