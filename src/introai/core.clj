(ns introai.core
  (:gen-class)
  (:require [introai.assignment2.run-game :refer [run-from-opts opt-parser]]
            [clojure.pprint :refer [pprint]]))


(def HELP-MESSAGE "
Command line args:
   -f, --file-name      <Filename>        path to runtime file
   -a, --alg            <ALG - NAME>      one of:   [user, greedy, greedy-search, a-star, rt-a-star]
   -T, --T              <float>           float value for T [0, 0.000001, 0.01]
   -L, --L              <int>             max expands for RT A*
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
