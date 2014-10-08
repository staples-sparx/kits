(ns kits.jetty
  (:import (java.io File FileInputStream InputStream OutputStream)
           (java.util Map HashMap)
           (javax.servlet.http HttpServletResponse HttpServletRequest)
           (org.eclipse.jetty.server Server Request ServerConnector)
           (org.eclipse.jetty.server.handler AbstractHandler)
           (org.eclipse.jetty.util.thread QueuedThreadPool ExecutorThreadPool)))

(set! *warn-on-reflection* true)

(defn copy
  ([in out]
     (copy in out 10240))
  ([^InputStream in ^OutputStream out chunk-size]
     (let [buf (make-array Byte/TYPE chunk-size)]
       (loop []
         (let [size (.read in buf)]
           (when (pos? size)
             (.write out ^bytes buf 0 size)
             (recur)))))))

(defn create [^long capacity]
  (HashMap. capacity))

(defn put! [^Map m key val]
  (.put m key val)
  m)

(defn base-array? [a] (.isArray ^Class (class a)))

(defn reply [^Request base-request ^HttpServletResponse servlet-resp clj-resp]
  (when servlet-resp
    (let [headers (:headers clj-resp)]
      (doto servlet-resp
        (.setContentType (or (get (:headers clj-resp) "content-type") "text/plain"))
        (.setStatus (or (:status clj-resp) 500)))
      (doseq [[header-name value] headers]
        (.setHeader servlet-resp (name header-name) value)))
    (.setHandled base-request true)
    (let [body (:body clj-resp)]
      (cond
        (= java.io.File (type body)) (with-open [in (FileInputStream. ^File body)]
                                       (copy in (.getOutputStream servlet-resp)))
        (base-array? body) (.write (.getOutputStream servlet-resp) ^bytes body)
        :else (doto (.getWriter servlet-resp)
                (.println (or body "No body")))))))

(defn reply-with-server-error [base-request servlet-resp error]
  (reply base-request servlet-resp
    {:status 500
     :body (str "Server Error # " error)}))

(defn sanitize-params [^Map servlet-params]
  (let [params (create (.size servlet-params))]
    (doseq [[name values] servlet-params]
      (put! params name (last values)))
    params))

(defn- handler [http-handler exception-handler]
  (proxy [AbstractHandler] []
    (handle [_ ^Request base-request request servlet-resp]
      (try
        (let [^HttpServletRequest request request
              method (.getMethod request)
              from-ip (.getRemoteAddr request)
              uri (.getRequestURI request)
              clj-request {:uri uri
                           :method method
                           :from-ip from-ip
                           :base-request request}
              servlet-params (.getParameterMap request)
              sanitized-params (sanitize-params servlet-params)]
          (try
            (let [clj-resp (http-handler clj-request sanitized-params)]
              (reply base-request servlet-resp clj-resp))
            (catch Exception e
              (let [error (exception-handler e clj-request sanitized-params)]
                (reply-with-server-error base-request servlet-resp error)))))
        (catch Exception e
          (let [error (exception-handler e nil nil)]
            (reply-with-server-error base-request servlet-resp error)))))))

(defn server [config http-handler exception-handler]
  (let [thread-pool (doto (QueuedThreadPool.)
                      (.setMaxThreads (int (:max-threads config)))
                      (.setMinThreads (int (:min-threads config))))
        server ^Server (Server. thread-pool)
        acceptors (int (or (:acceptors config) -1))
        selectors (int (or (:selectors config) -1))
        connector (doto ^Connector (ServerConnector. server acceptors selectors)
                        (.setPort (int (:port config))))]
    (doto ^Server server
      (.addConnector connector)
      (.setHandler (handler http-handler exception-handler)))))
