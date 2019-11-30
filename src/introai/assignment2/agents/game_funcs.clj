(ns introai.assignment2.agents.game-funcs
  (:gen-class)
  (:require
    [introai.assignment2.operators :as op]
    [clojure.core.strint :refer [<<]]
    [introai.utils.graphs :as gutils]
    [introai.utils.log :as log]
    [introai.assignment2.game-state :as gs]
    [introai.assignment2.graph-description :as gd]))


(def MAX_EXPAND-LIMIT 100000)
;
;(def gen-edge gutils/edge-from-state-target-node)
;
;(defn closest-shelter [graph-struct src shelters]
;  (apply min-key second (map #(vector % (gutils/dijkstra-dist graph-struct src %))
;                             shelters)))
;
;(defn people-can-save-at
;  [{graph-struct :structure, {shelters :shelters nodes :nodes} :props}
;   {src :agent-node remaining-people :remaining-people cur-time :time}
;   dest]
;
;  (let [[closest-shelter dist-close-shelter] (closest-shelter graph-struct dest shelters)
;        dest-dead-line (:dead-line (nodes dest))
;        shelter-dead-line (:dead-line (nodes closest-shelter))
;        distance-to-dest (gutils/dijkstra-dist graph-struct src dest)]
;
;    (let [savable-people
;          (if (or (< dest-dead-line (+ cur-time distance-to-dest))
;                  (< shelter-dead-line (+ cur-time distance-to-dest dist-close-shelter)))
;            0
;            (remaining-people dest))]
;      savable-people)))
;
;(defn heuristic [graph-desc state]
;  (let [savable-people
;        (apply + (map
;                   #(people-can-save-at graph-desc state %)
;                   (gd/remaining-nodes state)))]
;    (- (gd/all-people state) savable-people)))
;
;(defn goal? [{state :state}]
;  (gs/term? state))
;
;(defn next-state [{graph-struct :structure :as graph-desc} next-node state]
;  (op/edge graph-desc
;           (gen-edge graph-struct state next-node)
;           state))
;
;(defn term-state [graph-desc state]
;  (let [terminated-state (op/term graph-desc state)]
;    {
;     :g     (:score terminated-state)
;     :h     0
;     :state terminated-state
;     :op    (op/partial-term graph-desc)
;     }))
;
;(defn successor-state-map [graph-desc next-node state]
;  (let [new-state (next-state graph-desc next-node state)]
;    (if (gs/term? new-state)
;      (term-state graph-desc new-state)
;      {
;       :g     0
;       :h     (heuristic graph-desc new-state)
;       :state new-state
;       :op    (op/partial-edge graph-desc state (:agent-node new-state))
;       })))
;
;(defn edge-states [graph-desc neighbours state]
;  (map #(successor-state-map graph-desc % state) neighbours))
;

(defn agent-term? [cur-di-state agent] (gs/term? (gs/state-of cur-di-state agent)))
(defn agent-active? [cur-di-state agent] (not (agent-term? cur-di-state agent)))

(defn all-agents-term? [di-state agents]
  (every? #(agent-term? di-state %) agents))


(defn edge-ops [graph-desc di-state agent neighbours]
  (if (gd/time-over? graph-desc di-state agent)
    []
    (let [agent-state (gs/state-of di-state agent)]
      (map #(op/make-edge graph-desc agent-state %) neighbours))))

(defn gen-next-ops
  [graph-desc di-state agent]
  (if (agent-term? di-state agent)
    [(op/make-id)]
    (let [neighbours (gutils/state-successors graph-desc (gs/state-of di-state agent))]
      (let [next-ops
            (conj (edge-ops graph-desc di-state agent neighbours)
                  (op/make-term (gs/state-piece-of di-state agent :agent-node))
                  )]
        ;(doseq [next-state-map next-states]
        ;  (log/debug ">" (log/state-node next-state-map)))
        next-ops))))

;(defn gen-state-expander
;  [graph-desc]
;  #(expand graph-desc %))