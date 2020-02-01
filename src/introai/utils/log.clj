(ns introai.utils.log
  (:gen-class)
  (:require [clojure.core.strint :refer [<<]]
            [clojure.pprint :refer [print-table]]
            [introai.utils.const :as E]
            [introai.assignment4.game-state :as gs]))


(defn spy [thing & others] (do
                    (println "SPYY: " thing (if others (<< " - ~{others}") ""))
                    thing))

(defn info [& things]
  (apply println (cons "INFO:  " things)))

(defn info-nl [& things]
  (apply info things)
  (println ""))

(defn debug [& things]
  ;(apply println (cons "DEBG:  " things))
  nil
  )

(defn two-state
  [{remaining-people :remaining-people} {alice-state :state1 bob-state :state2 t :time id :id}]
  (info "Remaining people after move: " remaining-people)
  (info (<< "  * (~{t} - ~{id} - Alice)> ~{(into {} alice-state)}"))
  (info-nl (<< "  * (~{t} - ~{id} - B_o_b)> ~{(into {} bob-state)}")))

(defn turn [{t :time id :id :as di-state} agents-order]
  (let [agent-name (:name (first agents-order))
        agent-state (gs/state-piece-of di-state (first agents-order) :terminated)]
    (info (<< "(~{t} - ~{id})> [~{agent-name}] is now playing with state [~{agent-state}]"))))

(defn name-n-state [agent]
  {(:name agent) (into {} (:state agent))})

(def SUMMARY-FIELDS [:score :num-expands :time :saved :remaining-people
                     :final-node :num-edges-traversed])

(defn exe-summary [summary]
  (print-table [:field :val]
               (map #(identity {:field % :val (% summary)}) SUMMARY-FIELDS)))