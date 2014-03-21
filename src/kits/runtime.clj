(ns kits.runtime
  "Library to access run-time info from the JVM"
  (:require [clojure.string :as str])
  (:import (java.io File)
           (java.lang.management ManagementFactory
                                 OperatingSystemMXBean RuntimeMXBean)))

(def ^:private ^Runtime runtime (Runtime/getRuntime))

(def ^:private ^java.lang.management.RuntimeMXBean runtime-mx
  (ManagementFactory/getRuntimeMXBean))

(def ^:private ^OperatingSystemMXBean os-mx
  (ManagementFactory/getOperatingSystemMXBean))

(defn process-info
  "Returns {:pid pid :host host} of the running JVM"
  []
  (let [name (.getName ^java.lang.management.RuntimeMXBean runtime-mx)
        [process-id host] (str/split name #"@")]
    {:pid process-id
     :host host}))

(defn process-id
  "Return the pid of the running JVM"
  []
  (:pid (process-info)))

(defn host
  "Return the hostname of the running JVM"
  []
  (:host (process-info)))

(defn jvm-info []
  (let [^java.lang.management.RuntimeMXBean runtime-mx runtime-mx]
    {:name (.getName runtime-mx)
     :vm-name (.getVmName runtime-mx)
     :vm-vendor (.getVmVendor runtime-mx)
     :vm-version (.getVmVersion runtime-mx)
     :args (vec (.getInputArguments runtime-mx))
     :classpath (.getClassPath runtime-mx)
     :boot-classpath (.getBootClassPath runtime-mx)
     :system-properties (into {} (.getSystemProperties runtime-mx))}))

(defn thread-id
  "Returns the current thread id"
  []
  (.getId (Thread/currentThread)))

(defn thread-info
  "Returns the current thread id"
  []
  (let [t (Thread/currentThread)]
    {:id (.getId t)
     :name (.getName t)}))

(defn hostname
  "Returns the current host name"
  []
  (.getHostName (java.net.InetAddress/getLocalHost)))

(defn ip-address
  "Returns the string representation of the IP of the local host"
  []
  (.getHostAddress (java.net.InetAddress/getLocalHost)))

(defn load-average
  "Returns OS load average"
  []
  (.getSystemLoadAverage ^OperatingSystemMXBean os-mx))

(defn processor-count
  "Returns the number of processors available on the local machine"
  []
  (.getAvailableProcessors ^OperatingSystemMXBean os-mx))

(defn cpu-usage
  "Returns the CPU usage as a percentace of the total processing power"
  []
  (/ (load-average) (processor-count)))

(defn os-info
  "Return the name, version and architecture of the OS"
  []
  (let [^OperatingSystemMXBean os-mx os-mx]
    {:name (.getName os-mx)
     :version (.getVersion os-mx)
     :arch (.getArch os-mx)}))

(defn os-total-memory []
  (.getTotalPhysicalMemorySize
   ^com.sun.management.OperatingSystemMXBean os-mx))

(defn os-free-memory []
  (.getFreePhysicalMemorySize
   ^com.sun.management.OperatingSystemMXBean os-mx))

(defn os-memory
  "Returns info about the physical memory and swap"
  []
  (let [os-mx ^com.sun.management.OperatingSystemMXBean os-mx]
    {:physical {:total (os-total-memory)
                :free (os-free-memory)
                :committed (.getCommittedVirtualMemorySize os-mx)}
     :swap {:total (.getTotalSwapSpaceSize os-mx)
            :free (.getFreeSwapSpaceSize os-mx)}}))

(defn jvm-total-memory
  "Returns the total amount of memory in the Java virtual
   machine. (bytes)"
  []
  (.totalMemory ^Runtime runtime))

(defn jvm-free-memory
  "Returns the amount of free memory in the Java Virtual
   Machine (bytes)."
  []
  (.freeMemory ^Runtime runtime))

(defn jvm-max-memory
  "Returns the maximum amount of memory that the Java virtual machine
   will attempt to use. (bytes)"
  []
  (.maxMemory ^Runtime runtime))

(defn jvm-overall-memory
  "Returns info about global jvm memory usage"
  []
  {:total (jvm-total-memory)
   :free (jvm-free-memory)
   :max (.maxMemory ^Runtime runtime)})

(defn jvm-start-time []
  (.getStartTime ^RuntimeMXBean runtime-mx))

(defn uptime []
  (.getUptime ^RuntimeMXBean runtime-mx))

(defn disk-total [^String mount-point]
  (.getTotalSpace (File. mount-point)))

(defn disk-free [^String mount-point]
  (.getUsableSpace (File. mount-point)))

(defn disk-usage [^String mount-point]
  (let [mount (File. mount-point)]
    {:total (.getTotalSpace mount)
     :free (.getUsableSpace mount)}))

(defn add-jvm-shutdown-hooks [& fns]
  (doseq [f fns]
    (.addShutdownHook runtime (Thread. f))))
