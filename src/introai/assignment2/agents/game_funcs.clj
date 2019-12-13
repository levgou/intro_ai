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
(defn closest-shelter [graph-struct src shelters]
  (apply min-key second (map #(vector % (gutils/shortest-path-len graph-struct src %))
                             shelters)))

(defn can-reach-dest-on-time? [graph-desc node cur-time distance-to-dest]
  (< (gd/node-dead-line graph-desc node)
     (+ cur-time distance-to-dest)))

(defn can-reach-shelter-on-time? [{graph-struct :structure {shelters :shelters nodes :nodes} :props}
                                  node cur-time distance-to-dest]

  (let [[closest-shelter dist-close-shelter] (closest-shelter graph-struct node shelters)
        shelter-dead-line (:dead-line (nodes closest-shelter))]

    (< shelter-dead-line (+ cur-time distance-to-dest dist-close-shelter))))

(defn people-can-save-at
  [{graph-struct :structure remaining-people :remaining-people :as graph-desc}
   {src :agent-node}
   cur-time
   dest]

  (let [distance-to-dest (gutils/shortest-path-len graph-struct src dest)
        can-reach-dest (can-reach-dest-on-time? graph-desc dest cur-time distance-to-dest)
        can-reach-shelter (can-reach-shelter-on-time? graph-desc dest cur-time distance-to-dest)

        savable-people
        (if (and can-reach-dest can-reach-shelter)

          (if (= dest "5")
            (first [
                    (can-reach-dest-on-time? graph-desc dest cur-time distance-to-dest)
                    (remaining-people dest)])

            (remaining-people dest)
            )
          0)]

    savable-people))

(defn count-savable-people-other-nodes [graph-desc state cur-time]
  (apply + (map
             #(people-can-save-at graph-desc state cur-time %)
             (gd/remaining-nodes graph-desc))))

(defn carry-people-savable [graph-desc state cur-time]
  (if (can-reach-shelter-on-time? graph-desc (:agent-node state) cur-time 0)
    (:carrying state)
    0))

(defn count-savable-people [graph-desc state cur-time]
  (if (gs/term? state)
    0
    (let [at-others (count-savable-people-other-nodes graph-desc state cur-time)
          own (carry-people-savable graph-desc state cur-time)]
      (+ 2 3)
      (+ at-others own))))

(defn calc-term-penalty [state]
  (if (or (not (gs/term? state)) (gs/term-safe? state))
    0
    (- -2 (gs/died-with-agent-count state))))

(defn calc-giveup-penalty [state savable-people]
  (if (and (pos? savable-people) (gs/term? state))
    -1
    0))

(defn heuristic [graph-desc state cur-time]
  (let [savable-people (count-savable-people graph-desc state cur-time)
        term-penalty (calc-term-penalty state)
        giveup-penalty (calc-giveup-penalty state savable-people)]

    (let [h-val
          (+
            (if-not (gs/term? state) savable-people 0)
            term-penalty
            giveup-penalty)]
      (+ 2 3)
      h-val)))

(defn agent-heuristic [graph-desc di-state agent]
  (heuristic graph-desc (gs/state-of di-state agent) (:time di-state)))

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
(defn mid-edge? [graph-desc di-state agent] (->> (gs/state-of di-state agent)
                                                 :agent-node
                                                 (gutils/node-mid-edge? (:structure graph-desc))))

(defn all-agents-term? [di-state agents]
  (every? #(agent-term? di-state %) agents))


(defn edge-ops [graph-desc di-state agent neighbours]
  (if (gd/time-over? graph-desc di-state agent)
    []
    (let [agent-state (gs/state-of di-state agent)]
      (map #(op/make-edge graph-desc agent-state %) neighbours))))

(defn only-edge-op [graph-desc di-state agent]
  (op/make-edge graph-desc di-state
                (first (gutils/state-successors graph-desc (gs/state-of di-state agent)))))

(defn gen-next-ops
  [graph-desc di-state agent]
  (cond
    (agent-term? di-state agent) [(op/make-id)]
    (mid-edge? graph-desc di-state agent) [(only-edge-op graph-desc di-state agent)]

    :else
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