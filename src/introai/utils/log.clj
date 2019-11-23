(ns introai.utils.log
  (:gen-class)
  (:require [clojure.core.strint :refer [<<]]
            [clojure.pprint :refer [print-table]]
            ))

(defn spy [thing] (do (println "SPYY: " thing) thing))

(defn info [& things]
  (apply println (cons "INFO:  " things)))

(defn debug [& things]
  ;(apply println (cons "DEBG:  " things))
  nil
  )

(defn state-node
  [state-node]
  (let [state (:state state-node) f (+ (:g state-node) (:h state-node))]
    (<<
      "Node[~{(:id state)}]"
      "{:agent-node ~{(:agent-node state)}, "
      ":time ~{(:time state)}, "
      ":carrying ~{(:carrying state)}, "
      ":saved ~{(:saved state)}, "
      ":dead ~{(:dead state)}, "
      ":term? ~{(:terminated state)}, "
      ":remaining ~{(:remaining-people state)}, "
      ":g ~{(:g state-node)}, :h ~{(:h state-node)}, :f ~{f}}"
      )))

(def SUMMARY-FIELDS [:score :num-expands :time :saved :remaining-people
                     :final-node :num-edges-traversed :time-penalties])

(defn exe-summary [summary]
  (print-table [:field :val]
               (map #(identity {:field % :val (% summary)}) SUMMARY-FIELDS)))