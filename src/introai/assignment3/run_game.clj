(ns introai.assignment3.run-game
  (:gen-class)
  (:require
    [introai.assignment3.agents.game-funcs :refer [all-agents-term? agent-term?]]
    [introai.assignment3.agents.agent :refer [gen-agent]]
    [clojure.core.strint :refer [<<]]
    [nano-id.core :refer [nano-id]]
    [clojure.tools.cli :refer [parse-opts]]
    [introai.assignment3.read-bayes :refer [read-graph-from-file]]
    [introai.assignment3.bayes-network :refer [gen-b-net-t-0]]
    [introai.assignment3.main-loop :refer [main-loop]]
    ))

(def CMD-OPTS
  [
   ["-f" "--file-name   FILE-NAME" "path to runtime file"
    :id :file-name]

   ["-s" "--sample-size POS-INT" "amount of evidence consistent samples to take"
    :id :sample-size
    :default 10000
    :parse-fn #(Integer/parseInt %)]

   ["-h" "--help"]
   ])

(defn opt-parser [args]
  (:options (parse-opts args CMD-OPTS)))

(defn run-from-opts [opts]
  (let [graph-desc (read-graph-from-file (:file-name opts))
        bayes (gen-b-net-t-0 graph-desc (-> graph-desc :props :persist))]
    (main-loop graph-desc bayes (:sample-size opts))))
