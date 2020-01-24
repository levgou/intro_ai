(ns introai.core
  (:gen-class)
  (:require [introai.assignment4.run-game :refer [run-from-opts opt-parser]]
            [introai.assignment4.main-loop :refer [main-loop]]
            [clojure.pprint :refer [pprint]]))


(def HELP-MESSAGE "
Command line args:
   -f, --file-name      <Filename>             path to runtime file
   -s, --sample-size    <number>               sample size that agrees with evidence
")

(defn print-opts [opts]
  (println "Runtime configuration:")
  (println (apply str (repeat 100 "-")))
  (pprint  opts)
  (println (apply str (repeat 100 "-"))))

(defn -main
  [& args]
  (let [opts (opt-parser args)]
    (print-opts opts)
    (if (:help opts)
      (println HELP-MESSAGE)
      (run-from-opts opts))))
