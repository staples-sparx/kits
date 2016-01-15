(ns kits.prometheus
  "Bridge to Prometheus's java library. "
  (:require [clojure.string :as s])
  (:import [io.prometheus.client Gauge Counter Histogram]
           [io.prometheus.client.hotspot DefaultExports]
           [io.prometheus.client CollectorRegistry]
           [io.prometheus.client.exporter.common TextFormat]
           [java.io StringWriter]))

(defn init []
  (DefaultExports/initialize))

(defn ->prometheus-name [metric-name]
  (-> metric-name
      name
      (s/replace #"-" "_")))

(defn create-histogram [metric-name help label-names {:keys [start width count]
                                                      :or {start 0}
                                                      :as bucket-config}]
  (-> (Histogram/build)
      (.name (->prometheus-name metric-name))
      (.labelNames (into-array (map ->prometheus-name label-names)))
      (.linearBuckets (double start) (double width) (int count))
      (.help help)
      .register))

(defn registry []
  (CollectorRegistry/defaultRegistry))

(defn register
  "Register a response from the fetch step"
  [^Histogram histogram elapsed-ms label-values]
  (-> histogram
      (.labels (into-array (map str label-values)))
      (.observe (double elapsed-ms))))

(defn metrics-endpoint [request]
  (let [writer (StringWriter.)]
    (TextFormat/write004 writer (.metricFamilySamples (registry)))
    {:status  200
     :headers {"Content-Type" TextFormat/CONTENT_TYPE_004}
     :body    (.toString writer)}))

(defn clear-default-registry []
  (.clear (registry)))
