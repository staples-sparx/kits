(ns kits.utils.shell
  "Miscellaneous utilities."
  (:require [clojure.java.shell    :as shell]
  ) )

(set! *warn-on-reflection* true)

(def ^:const os-shell "/bin/bash")

(defn run-cmd-in-shell 
  "Run a command represented as a string in an OS shell (e.g. /bin/bash).
  Example: 'ls -ldF *'  "
  [cmd-str]
  (let [result (shell/sh os-shell "-c" cmd-str)]
    (if (= 0 (:exit result))
      result
      (throw (RuntimeException. 
               (str "run-cmd-in-shell: clojure.java.shell/sh failed. \n" 
                    "exit status:" (:exit result) "\n"
                    "stderr:"      (:err  result) "\n"
                    "result:"      (:out  result) "\n" ))) )))

