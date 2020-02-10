(ns introai.core
  (:gen-class)
  (:require
    [clojure.pprint :refer [pprint]]
    [clojure.tools.cli :refer [parse-opts]]
    [introai.assignment4.simulate :as sim]
    ))


(def HELP-MESSAGE "
Command line args:
   -f, --file-name      <Filename>             path to runtime file
   -s, --simulate                              run simulation
   -e, --enum                                  enumerate belief space
")


(def CMD-OPTS
  [
   ["-f" "--file-name   FILE-NAME" "path to runtime file"
    :id :file-name]

   ["-s" "--simulate"
    :id :simulate
    :default false]

   ["-e" "--enum"
    :id :enum
    :default false]

   ["-h" "--help"]
   ])

(defn opt-parser [args]
  (:options (parse-opts args CMD-OPTS)))

(defn print-opts [opts]
  (println "Runtime configuration:")
  (println (apply str (repeat 100 "-")))
  (pprint opts)
  (println (apply str (repeat 100 "-"))))

(defn -main
  [& args]

  ;(sim/run-simu "/Users/levgour/skool/introai/ass4/intro_ai/test/introai/resources/mini.aig")
  (let [opts (opt-parser args)]
    (print-opts opts)
  (if (:help opts)
    (println HELP-MESSAGE)

    (if (:simulate opts)
      (sim/run-simu (:file-name opts))
      (sim/enumerate (:file-name opts))))))
