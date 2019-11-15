(ns introai.assignment1.agents.game-funcs
  (:gen-class)
  (:require
    [loom.graph :as graph]
    [loom.alg :as alg]
    [introai.assignment1.operators :as op]
    [introai.utils.graphs :as gutils]
    [introai.assignment1.graph-description :as gd]
    ))

(def gen-edge gutils/edge-from-state-target-node)

(defn closest-shelter [graph-struct src shelters]
  (apply min-key second (map
                          #([% (gutils/dijkstra-dist graph-struct src %)])
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
  (apply + (map
             #(people-can-save-at graph-desc state %)
             (gd/remaining-nodes state))))

(defn goal? [{state :state}]
  (:terminated state))

(defn next-state [{graph-struct :structure} next-node state]
  (let [edge-op (op/edge graph-struct (gen-edge graph-struct state next-node) state)]
    (edge-op state)))

(defn term-state [state]
  {
   :g     (+ (vals (:remaining-people state)))
   :h     0
   :state (op/term state)
   })

(defn successor-state-map [graph-desc next-node state]
  (let [new-state (next-state graph-desc next-node state)]
    {
     :g     0
     :h     (heuristic graph-desc new-state)
     :state new-state
     }))

(defn expand
  [graph-desc state]
  (if (:terminated state)
    {}
    (let [neighbours (gutils/state-successors graph-desc state)]
      (conj
        (map #(successor-state-map graph-desc % state) neighbours)
        (term-state state)))))

(defn gen-state-expander
  [graph-desc]
  #(expand graph-desc %))