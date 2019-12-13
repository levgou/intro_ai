(ns introai.assignment2.agents.max-max
  (:gen-class)
  (:require
    [introai.assignment2.agents.game-funcs :refer [gen-next-ops agent-term? agent-heuristic]]
    [introai.assignment2.game-state :as gs]
    [introai.utils.log :as log]
    [introai.utils.const :refer [INF -INF]]
    [introai.utils.const :as E]
    [nano-id.core :refer [nano-id]]
    [introai.utils.graphs :as gutils]
    ))


(declare max-value)
(declare assoc-res-with-max-val)
(declare assoc-op-with-res)

(defrecord MaxMaxState [graph-desc di-state heuristic agent-order]
  Object
  (toString [x] (str (into {} x))))
(defmethod print-method MaxMaxState [x ^java.io.Writer w] (.write w (str x)))

(defn rev-order [maxmax-state]
  (update maxmax-state :agent-order reverse))

(defn minimizers-turn [minimax-state] (:agent-order minimax-state))

(defn inc-depth [{agent-order :agent-order graph-desc :graph-desc di-state :di-state} depth]
  (let [op-agent (first agent-order)
        mid-edge (gutils/node-mid-edge? graph-desc di-state op-agent)
        terminated (agent-term? di-state op-agent)]
    ;(log/info (:name op-agent) depth)
    (+ depth (if (or mid-edge terminated) 0 1))))

(defn progress-tick [maxmax-props op-agent ops-and-results]
  (if (not= op-agent (:time-progressor maxmax-props))
    ops-and-results
    (map #(update-in % [:di-state :time] inc) ops-and-results)))

(defn update-graph-state [maxmax-state graph-desc di-state]
  (assoc maxmax-state :graph-desc graph-desc :di-state di-state))

(defrecord MaxMaxNodeProps [depth maxifier1 maxifier2 time-progressor agent]
  Object
  (toString [x] (str (into {} x))))
(defmethod print-method MaxMaxNodeProps [x ^java.io.Writer w] (.write w (str x)))

(defn both-agents-terminated [di-state agent-order]
  (log/debug "Both agents terminated!")
  (and
    (agent-term? di-state (first agent-order))
    (agent-term? di-state (second agent-order))
    ))

(def MAX-DEPTH 9)
(defn cutoff-maxmax [maxmax-props]
  (log/debug "Depth cutoff!")
  (< MAX-DEPTH (:depth maxmax-props)))

(defn term-search? [{di-state :di-state agent-order :agent-order} maxmax-props]
  (or
    (both-agents-terminated di-state agent-order)
    (cutoff-maxmax maxmax-props)))

(defn calc-agent-score [state]
  (-
    (:saved state)

    (if (= (:terminated state) E/TERMINATED-UNSAFELY) 2 0)
    (-> state :dead E/DIED-IN-CITY)
    (* 2 (-> state :dead E/DIED-WITH-AGENT))
    ))

(defn next-ops [{graph-desc :graph-desc di-state :di-state} op-agent]
  (into []
        (map #(identity {:op %})
             (gen-next-ops graph-desc di-state op-agent))))

(defn own-heuristic
  [{graph-desc :graph-desc di-state :di-state :as maxmax-state} agent]
  (let [other (gs/other-agent maxmax-state agent)]

    (agent-heuristic graph-desc di-state agent)

    ))

(defn stat-eval
  "todo: could heuristic be better than actual score?"
  [{di-state :di-state agent-order :agent-order heuristic :heuristic :as maxmax-state}
   maxmax-props
   agent]


  (let [agent-state (gs/state-of di-state agent)]

    (if (both-agents-terminated di-state agent-order)
      ;(log/spy
      {:score [(calc-agent-score agent-state) (calc-agent-score (gs/other-agent-state maxmax-state agent))]
       :org   "G" :depth (:depth maxmax-props) :id (nano-id 5) :agent (str agent)}
      ;)
      ;(log/spy
      {:score [(heuristic maxmax-state agent) (heuristic maxmax-state (gs/other-agent-state maxmax-state agent))]
       :org   "H" :depth (:depth maxmax-props) :id (nano-id 5) :agent (str agent)}
      ;)
      )))

(defn assoc-op-with-res [graph-desc di-state agent map-with-op]
  (let [[new-graph-desc new-di-state] ((:op map-with-op) graph-desc di-state agent)]
    (assoc map-with-op :graph-desc new-graph-desc :di-state new-di-state)))

(defn assoc-ops-with-res
  [{graph-desc :graph-desc di-state :di-state} op-agent ops]
  (into []
        (map #(assoc-op-with-res graph-desc di-state op-agent %) ops)))

(defn assoc-res-with-max-val
  [{op :op graph-desc :graph-desc di-state :di-state :as m} maxmax-state maxmax-props]

  (let [new-maxmax-state (update-graph-state maxmax-state graph-desc di-state)
        res-and-max-val (max-value (rev-order new-maxmax-state) maxmax-props)]
    (-> m
        (assoc :max-val (:max-val res-and-max-val))
        (assoc :tail res-and-max-val))
    ;res-and-max-val
    ))


(defn current-maxifier [maxmax-state maxmax-props]
  (if (-> maxmax-state :agent-order first :name (= "Alice"))
    (:maxifier1 maxmax-props)
    (:maxifier2 maxmax-props)))

(defn res-and-max-val [maxmax-state maxmax-props res]
  (assoc-res-with-max-val
    res maxmax-state
    (assoc maxmax-props :depth (inc-depth maxmax-state (:depth maxmax-props)))))

(defn results-and-max-values [maxmax-state maxmax-props ops-and-results]
  (into [] (map #(res-and-max-val maxmax-state maxmax-props %) ops-and-results)))

(defn next-state-max
  [maxmax-state ops-and-results maxmax-props]

  (let [maxifier (current-maxifier maxmax-state maxmax-props)
        results-and-max-vals (results-and-max-values maxmax-state maxmax-props ops-and-results)]
    (last (sort-by maxifier results-and-max-vals)))
  ;
  ;(loop [[res & others] ops-and-results
  ;       max-res {:max-val {:score [-INF -INF]}}]
  ;
  ;  (let [res-and-val (assoc-res-with-max-val
  ;                      res maxmax-state
  ;                      (assoc maxmax-props :depth (inc-depth maxmax-state (:depth maxmax-props))))
  ;
  ;        nex-max-res (last (sort-by maxifier [max-res res-and-val]))]
  ;
  ;    (if (nil? others)
  ;      res-and-val
  ;      (recur others nex-max-res))))
  ;
  ;
  )

(defn max-value
  [maxmax-state maxmax-props]

  (let [[agent op-agent] [(:agent maxmax-props) (first (:agent-order maxmax-state))]]

    (if (term-search? maxmax-state maxmax-props)
      (let [di-score (stat-eval maxmax-state maxmax-props agent)]
        {:max-val di-score :tail nil})

      (let [ops (next-ops maxmax-state op-agent)
            ops-and-results (assoc-ops-with-res maxmax-state op-agent ops)
            ops-and-res-tick (progress-tick maxmax-props op-agent ops-and-results)
            next-op-and-score (next-state-max maxmax-state ops-and-res-tick maxmax-props)]
        {:max-val (:max-val next-op-and-score) :tail next-op-and-score}
        ))))

(defn tail-summary
  ([m]
   (tail-summary m []))

  ([m state-vec]
   (loop [cur-m m cur-state-vec state-vec iter 1]
     ;(println [
     ;          (-> cur-m :di-state :state1 :agent-node)
     ;          (-> cur-m :di-state :state2 :agent-node)
     ;          ]
     ;         )
     (if (nil? cur-m)
       cur-state-vec

       (recur
         (-> cur-m :tail :tail)
         (conj cur-state-vec
               [iter
                [
                 (-> cur-m :di-state :state1 :agent-node)
                 (-> cur-m :di-state :state1 :terminated)
                 (-> cur-m :di-state :state2 :agent-node)
                 (-> cur-m :di-state :state2 :terminated)
                 ]])
         (inc iter)
         )))))

(defn identity-maxifier [res-and-val]
  (-> res-and-val :max-val :score))

(defn rev-identity-maxifier [res-and-val]
  (into [] (reverse (identity-maxifier res-and-val))))

(defn max-max [graph-desc di-state agent-order]
  (log/info (str (first agent-order)) ">>> " (:remaining-people graph-desc))

  (let [time-progressor (first (filter #(= (:name %) "Bob") agent-order))
        initial-maxmax-state (MaxMaxState. graph-desc di-state own-heuristic agent-order)
        initial-maxmax-props (MaxMaxNodeProps. 0 identity-maxifier rev-identity-maxifier time-progressor (first agent-order))
        op-agent (first agent-order)]

    (let [ops (next-ops initial-maxmax-state op-agent)
          ops-and-results (assoc-ops-with-res initial-maxmax-state op-agent ops)
          maps-with-max-vals (into [] (map #(assoc-res-with-max-val % initial-maxmax-state initial-maxmax-props) ops-and-results))]

      (doseq [m maps-with-max-vals]
        (log/info  (str (first agent-order)) " &>>" (:max-val m) (into {} (:op m)) (tail-summary m)))

      (:op

        (last (sort-by identity-maxifier maps-with-max-vals))
        ;(apply max-key #(-> % :max-val :score) maps-with-max-vals)

        ))))