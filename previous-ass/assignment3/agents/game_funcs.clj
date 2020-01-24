(ns introai.assignment3.agents.game-funcs
  (:gen-class)
  (:require
    [introai.assignment3.operators :as op]
    [clojure.core.strint :refer [<<]]
    [introai.utils.graphs :as gutils]
    [introai.utils.log :as log]
    [introai.assignment3.game-state :as gs]
    [introai.assignment3.graph-description :as gd]))


(def MAX_EXPAND-LIMIT 100000)

(defn closest-shelter [graph-struct src shelters]
  (apply min-key second (map #(vector % (gutils/shortest-path-len graph-struct src %))
                             shelters)))

(defn can-reach-dest-on-time? [graph-desc node cur-time distance-to-dest]
  (>= (gd/node-dead-line graph-desc node)
      (+ cur-time distance-to-dest)))

(defn can-reach-shelter-on-time? [{graph-struct :structure {shelters :shelters nodes :nodes} :props}
                                  node cur-time distance-to-dest]

  (let [[closest-shelter dist-close-shelter] (closest-shelter graph-struct node shelters)
        shelter-dead-line (:dead-line (nodes closest-shelter))]

    (>= shelter-dead-line (+ cur-time distance-to-dest dist-close-shelter))))


(defn people-can-save-at
  [{graph-struct :structure remaining-people :remaining-people :as graph-desc}
   {src :agent-node}
   cur-time
   dest]

  (let [distance-to-dest (gutils/shortest-path-len graph-struct src dest)

        savable-people
        (if (and (can-reach-dest-on-time? graph-desc dest cur-time distance-to-dest)
                 (can-reach-shelter-on-time? graph-desc dest cur-time distance-to-dest))
          (remaining-people dest)
          0)]

    savable-people))

(defn count-savable-people-other-nodes [graph-desc state cur-time]
  (let [savables (zipmap (gd/remaining-nodes graph-desc)
                         (map
                           #(people-can-save-at graph-desc state cur-time %)
                           (gd/remaining-nodes graph-desc)))]

    (assoc savables :all (apply + (vals savables)))
    ))

(defn carry-people-savable [graph-desc state cur-time]
  (if (can-reach-shelter-on-time? graph-desc (:agent-node state) cur-time 0)
    (:carrying state)
    0))

(defn count-savable-people [graph-desc state cur-time]
  (if (gs/term? state)
    0
    (let [at-others (count-savable-people-other-nodes graph-desc state cur-time)
          own (carry-people-savable graph-desc state cur-time)]
      {:all       (+ (:all at-others) own)
       :at-others at-others
       :own       own}
      )))

(defn calc-term-penalty [state]
  (if (or (not (gs/term? state)) (gs/term-safe? state))
    0
    (- -2 (gs/died-with-agent-count state))))

(defn calc-giveup-penalty [state savable-people]
  (if (and (pos? savable-people) (gs/term? state))
    -1
    0))

(defn suicide-state-penalty [graph-desc state cur-time]
  (let [can-shelter (can-reach-shelter-on-time? graph-desc (:agent-node state) cur-time 0)
        people-carry (:carrying state)]

    (if can-shelter
      0
      (- -2 (* 2 people-carry)))))

(defn heuristic [graph-desc state cur-time]
  (if (gs/term? state)
    {:val 0 :sid (:id state)}

    (let [savable-people (count-savable-people graph-desc state cur-time)
          suicide-penalty (suicide-state-penalty graph-desc state cur-time)
          ;term-penalty (calc-term-penalty state)
          ;giveup-penalty (calc-giveup-penalty state savable-people)
          ]

      (let [h-val (+
                    (if-not (gs/term? state) (:all savable-people) 0)
                    suicide-penalty
                    ;term-penalty
                    ;giveup-penalty
                    )]

        (assoc savable-people :val h-val :sp suicide-penalty :sid (:id state))))))

(defn agent-heuristic [graph-desc di-state agent]
  (heuristic graph-desc (gs/state-of di-state agent) (:time di-state)))

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

  (when (> (:time di-state) 5)
    (+ 2 3))

  (cond
    (agent-term? di-state agent)
    [(op/make-id)]

    (mid-edge? graph-desc di-state agent)
    [(only-edge-op graph-desc di-state agent)]

    (gd/time-over? graph-desc di-state agent)
    [(op/make-term (gs/state-piece-of di-state agent :agent-node))]

    :else
    (let [neighbours (gutils/state-successors graph-desc (gs/state-of di-state agent))]
      (let [next-ops
            (conj (edge-ops graph-desc di-state agent neighbours)
                  (op/make-term (gs/state-piece-of di-state agent :agent-node))
                  )]
        ;(doseq [next-state-map next-states]
        ;  (log/debug ">" (log/state-node next-state-map)))
        next-ops))))


(defn player-max-sort-key
  "sort key - that will maximize the score of the first agent - second agent"
  [m-ogdmt]
  (-> m-ogdmt :evl :scores ((fn [[p1 p2]] (- p1 p2)))))

(defn player-min-sort-key
  "sort key - that will minimize the score of the first agent - second agent"
  [m-ogdmt]
  (- (player-max-sort-key m-ogdmt)))

(defn identity-maxifier
  "sort key - that will maximize the score of the first agent"
  [m-ogdmt]
  (-> m-ogdmt :evl :scores))

(defn rev-identity-maxifier
  "sort key - that will maximize the score of the second agent"
  [res-and-val]
  (into [] (reverse (identity-maxifier res-and-val))))

(defn coop-maxifier
  "sort key to max the sum of scores"
  [m-ogdmt]
  (->> m-ogdmt :evl :scores (apply +)))
