(ns kits.load-driver.sessions)

(defn ->session [steps]
  {:state nil
   :remaining-steps steps})

(defn- run-step [next-step state enqueue-response]
  (let [{:keys [state 
                status
                transit-start
                transit-end
                step-name
                error
                exception]} (next-step state)]
    (enqueue-response {:status status
                       :transit-start transit-start
                       :transit-end transit-end
                       :step-name step-name
                       :error error
                       :exception exception})
    [error state]))

(defn- finish-session [state on-session-completed sleep-millis]
  (when sleep-millis
    (Thread/sleep sleep-millis))
  (on-session-completed state)
  nil)


;;; Schedulers

(defn ->round-robin 
  "For running one step from each session interleaved with other sessions"
  [& [{:keys [ms-between-step ms-between-session]}]]
  (fn [{:keys [state remaining-steps] :as session}
       enqueue-response
       on-session-completed]
    (let [[next-step & rest] remaining-steps
          [error state] (run-step next-step state enqueue-response)]
      (when ms-between-step
        (Thread/sleep ms-between-step))
      (if (and (seq rest) (not error))
        (assoc session
          :state state
          :remaining-steps rest)
        (finish-session state on-session-completed ms-between-session)))))

(defn ->run-all 
  "For running all requests in the session at once"
  [& [{:keys [ms-between-step ms-between-session]}]]
  (fn [{:keys [state remaining-steps] :as session}
       enqueue-response
       on-session-completed]
    (loop [[next-step & rest] remaining-steps
           state state]
      (if-not next-step
        (finish-session state on-session-completed ms-between-session)
        (let [[error state] (run-step next-step state enqueue-response)]
          (when ms-between-step
            (Thread/sleep ms-between-step))
          (if error
            (finish-session state on-session-completed ms-between-session)
            (recur rest state)))))))
