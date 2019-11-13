(ns introai.core
  (:gen-class)
  (:require [introai.assignment1.run-game :refer [run-from-file]]))

(defn -main
  [& args]
  (run-from-file "D:\\skool\\ai_intro\\test\\introai\\resources\\example_graph.aig"))
