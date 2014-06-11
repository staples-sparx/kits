(ns kits.utils.shell-test
  (:require  [kits.utils.shell     :as shell ] )
  (:use      [clojure.test]) )

(set! *warn-on-reflection* false)

(deftest run-cmd-in-shell
  (testing "first test - passing" (do
    (let [result (shell/run-cmd-in-shell "ls -ldF *")]
      (when false
        (println "(:out result)" )
        (println  (:out result)  ) )
      (is (= 0 (:exit result))) )))

  (testing "second test -failing" (do
    (is (thrown? RuntimeException (shell/run-cmd-in-shell "LLLls -ldF *"))) ))
)

