(ns introai.core
  (:gen-class)
  (:require
    [clojure.pprint :refer [pprint]]
    [introai.assignment4.simulate :as sim]
    ))


(def HELP-MESSAGE "
Command line args:
   -f, --file-name      <Filename>             path to runtime file
")

(defn print-opts [opts]
  (println "Runtime configuration:")
  (println (apply str (repeat 100 "-")))
  (pprint opts)
  (println (apply str (repeat 100 "-"))))

(defn -main
  [& args]

  (sim/run-simu "/Users/levgour/skool/introai/ass4/intro_ai/test/introai/resources/mini.aig")
  ;(let [opts (opt-parser args)]
  ;  (print-opts opts)
  ;(if (:help opts)
  ;  (println HELP-MESSAGE)
  ;(run-from-opts opts)
  0)+
