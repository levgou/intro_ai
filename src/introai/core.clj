(ns introai.core
  (:gen-class)
  (:require [introai.assignment1.run-game :refer [run-from-opts opt-parser]]
            [clojure.pprint :refer [pprint]]))

(defn print-opts [opts]
  (println "Runtime configuration:")
  (println (apply str (repeat 100 "-")))
  (pprint  opts)
  (println (apply str (repeat 100 "-"))))

(defn -main
  [& args]
  (let [opts (opt-parser args)]
    (print-opts opts)
    (run-from-opts opts)))
