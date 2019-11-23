(ns introai.assignment1.agents.game-funcs
  (:gen-class)
  (:require
    [introai.assignment1.operators :as op]
    [introai.utils.graphs :as gutils]
    [introai.assignment1.graph-description :as gd]
    [introai.utils.log :as log]
    [introai.assignment1.game-state :as gs]
    [introai.utils.enums :as E]))


(def MAX_EXPAND-LIMIT 100000)

(def gen-edge gutils/edge-from-state-target-node)

(defn closest-shelter [graph-struct src shelters]
  (apply min-key second (map
                          #(vector % (gutils/dijkstra-dist graph-struct src %))
                          shelters)))

(defn people-can-save-at
  [
   {graph-struct :structure, {shelters :shelters nodes :nodes} :props}
   {src :agent-node remaining-people :remaining-people cur-time :time}
   dest
   ]

  (let [
        [_ dist-close-shelter] (closest-shelter graph-struct src shelters)
        dead-line (:dead-line (nodes dest))
        ]

    (if (< dead-line (+ cur-time
                        (gutils/dijkstra-dist graph-struct src dest)
                        dist-close-shelter))
      0
      (remaining-people dest))))

(defn heuristic [graph-desc state]
  (let [savable-people
        (apply + (map
                   #(people-can-save-at graph-desc state %)
                   (gd/remaining-nodes state)))]
    (- (gd/all-people state) savable-people)))

(defn goal? [{state :state}]
  (gs/term? state))

(defn next-state [{graph-struct :structure :as graph-desc} next-node state]
  (op/edge graph-desc
           (gen-edge graph-struct state next-node)
           state))

(defn term-state [graph-desc state]
  (let [terminated-state (op/term graph-desc state)]
    {
     :g     (:score terminated-state)
     :h     0
     :state terminated-state
     :op    (op/partial-term graph-desc)
     }))

(defn successor-state-map [graph-desc next-node state]
  (let [new-state (next-state graph-desc next-node state)]
    (if (gs/term? new-state)
      (term-state graph-desc new-state)
      {
       :g     0
       :h     (heuristic graph-desc new-state)
       :state new-state
       :op    (op/partial-edge graph-desc state (:agent-node new-state))
       })))

(defn edge-states [graph-desc neighbours state]
  (map #(successor-state-map graph-desc % state) neighbours))

(defn expand
  [graph-desc state]
  (if (gs/term? state)
    {}
    (let [neighbours (gutils/state-successors graph-desc state)]
      (let [next-states (conj (edge-states graph-desc neighbours state) (term-state graph-desc state))]
        (doseq [next-state-map next-states]
          (log/debug ">" (log/state-node next-state-map)))
        next-states))))

(defn gen-state-expander
  [graph-desc]
  #(expand graph-desc %))