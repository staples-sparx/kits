(ns kits.jetty
  (:import
   java.io.InputStream
   java.io.OutputStream
   java.io.File
   java.io.FileInputStream
   java.util.Map
   java.util.HashMap
   javax.servlet.http.HttpServletRequest
   javax.servlet.http.HttpServletResponse
   org.eclipse.jetty.server.Server
   org.eclipse.jetty.server.Handler
   org.eclipse.jetty.server.Request
   org.eclipse.jetty.util.thread.QueuedThreadPool
   org.eclipse.jetty.util.thread.ExecutorThreadPool
   org.eclipse.jetty.server.nio.SelectChannelConnector))

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

(deftype JettyAdapter [^{:unsynchronized-mutable true} server application-handler
                       exception-handler]
  Handler
  (handle [this target base-request request servlet-resp]
    (try
      (let [^HttpServletRequest request request
            method (.getMethod request)
            from-ip (.getRemoteAddr request)
            clj-request {:uri target
                         :method method
                         :from-ip from-ip
                         :base-request request}
            servlet-params (.getParameterMap request)
            sanitized-params (sanitize-params servlet-params)]
        (try
          (let [clj-resp (application-handler clj-request sanitized-params)]
            (reply base-request servlet-resp clj-resp))
          (catch Exception e
            (let [error (exception-handler e clj-request sanitized-params)]
              (reply-with-server-error base-request servlet-resp error)))))
      (catch Exception e
        (let [error (exception-handler e nil nil)]
          (reply-with-server-error base-request servlet-resp error)))))

  (setServer [this new-server]
    (let [previous server]
      (when (and previous (not= previous new-server))
        (-> ^Server server
          (.getContainer)
          (.removeBean ^Handler this)))
      (set! server new-server)
      (when (and new-server (not= previous new-server))
        (-> ^Server server
          (.getContainer)
          (.addBean ^Handler this)))))
  (getServer [this]
    server)
  (destroy [this]
    (when server
      (-> ^Server server
        (.getContainer)
        (.removeBean ^Handler this))))

  (start [this])
  (stop [this]))

(defn server [config http-handler exception-handler]
  (let [thread-pool (doto (QueuedThreadPool.)
                      (.setMaxThreads (int (:max-threads config)))
                      (.setMinThreads (int (:min-threads config))))
        connector (doto (SelectChannelConnector.)
                    (.setAcceptors (int (:acceptors config)))
                    (.setPort (int (:port config)))) ]
    (doto (Server.)
                 (.setThreadPool thread-pool)
                 (.addConnector connector)
                 (.setHandler (JettyAdapter. nil http-handler exception-handler)))))
