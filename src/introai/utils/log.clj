(ns introai.utils.log
  (:gen-class)
  (:require [clojure.core.strint :refer [<<]]
            [clojure.pprint :refer [print-table]]
            [introai.utils.enums :as E]))

(defn spy [thing] (do (println "SPYY: " thing) thing))

(defn info [& things]
  (apply println (cons "INFO:  " things)))

(defn debug [& things]
  (apply println (cons "DEBG:  " things))
  nil
  )

(defn op-description [unresolved-op]
  (let [op (:op unresolved-op)]
    {
     :agent-name   (-> unresolved-op :agent :name)
     :name         (:op-type op)
     :src-dest     [(:src op) (:dest op)]
     :resolve-time (:resolve-time unresolved-op)
     }))

(defn agent-description [agent]
  {
   :name   (:name agent)
   :vertex (-> agent :state :agent-node)
   :state (-> agent :state :terminated)
   :carry (-> agent :state :carrying)
   })

(defn iteration [time idle-agents ops-in-progress graph-desc]
  (let [agents-desc (into '() (map agent-description idle-agents))
        ops-desc (into '() (map op-description ops-in-progress))
        remaining (:remaining-people graph-desc)]
    (info (<< "(~{time})> just-updated-agents: ~{agents-desc} ; next-ops: ~{ops-desc} ; remaining: ~{remaining}"))))

(defn name-n-state [agent]
  (assoc (into {} (:state agent)) :name (:name agent)))

(defn end [graph-desc agents]
  (let [agents-info (into [] (map name-n-state agents))]

    (info "----------- E-N-D -----------")
    (info "Remaining people: " (:remaining-people graph-desc))
    (doseq [agent-info agents-info] (info agent-info))
    (info "----------- D-N-E -----------")))

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