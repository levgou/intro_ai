(ns introai.core
  (:gen-class)
  (:require [introai.assignment3.run-game :refer [run-from-opts opt-parser]]
            [clojure.pprint :refer [pprint]]))


(def HELP-MESSAGE "
Command line args:
   -f, --file-name      <Filename>             path to runtime file
   -a, --alg            <ALG - NAME>           one of:   [minmax, semi-coop, coop]
   -c --cutoff-depth    <POSITIVE-INTEGER>     depth of game tree allowed
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
