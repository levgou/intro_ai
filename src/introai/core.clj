(ns introai.core
  (:gen-class)
  (:require
    [clojure.pprint :refer [pprint]]
    [introai.assignment4.read-graph :as rg]
    [introai.assignment4.belief-space :as bs]

    ))


(def HELP-MESSAGE "
Command line args:
   -f, --file-name      <Filename>             path to runtime file
   -s, --sample-size    <number>               sample size that agrees with evidence
")

(defn print-opts [opts]
  (println "Runtime configuration:")
  (println (apply str (repeat 100 "-")))
  (pprint opts)
  (println (apply str (repeat 100 "-"))))

(defn -main
  [& args]
  (let
    [
     G (rg/read-graph-from-file "/Users/levgour/skool/introai/ass4/intro_ai/test/introai/resources/mini.aig")
     B (bs/init-belief G)
     ]
    (bs/expand-init-belief B))


  ;(let [opts (opt-parser args)]
  ;  (print-opts opts)
  ;(if (:help opts)
  ;  (println HELP-MESSAGE)
  ;(run-from-opts opts)
  0)
