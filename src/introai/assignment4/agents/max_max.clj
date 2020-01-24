(ns introai.assignment4.agents.max-max
  (:gen-class)
  (:require
    [introai.assignment4.agents.game-funcs :refer [gen-next-ops agent-term? agent-heuristic]]
    [introai.assignment4.game-state :as gs]
    [introai.assignment4.agents.utils :refer [tail-summary-str calc-agent-score both-agents-terminated progress-tick]]
    [introai.utils.log :as log]
    [introai.utils.const :refer [INF -INF]]
    [introai.utils.const :as E]
    [nano-id.core :refer [nano-id]]
    [introai.utils.graphs :as gutils]
    [introai.assignment4.graph-description :as gd]))


(declare max-value)
(declare assoc-res-with-max-val)
(declare assoc-op-with-res)

(defrecord MaxMaxState [graph-desc di-state heuristic agent-order]
  Object
  (toString [x] (str (select-keys x [:agent-order]))))
(defmethod print-method MaxMaxState [x ^java.io.Writer w] (.write w (str "MMS:" x)))

(defn rev-order [maxmax-state]
  (update maxmax-state :agent-order reverse))

(defn inc-depth [{agent-order :agent-order graph-desc :graph-desc di-state :di-state} depth]
  (let [op-agent (first agent-order)
        mid-edge (gutils/node-mid-edge? graph-desc di-state op-agent)
        terminated (agent-term? di-state op-agent)]
    (+ depth (if (or mid-edge terminated) 0 1))))

(defn update-graph-state [maxmax-state graph-desc di-state]
  (assoc maxmax-state :graph-desc graph-desc :di-state di-state))

(defrecord MaxMaxNodeProps [depth maxifier1 maxifier2 time-progressor agent]
  Object
  (toString [x] (str (select-keys x [:depth :agent]))))
(defmethod print-method MaxMaxNodeProps [x ^java.io.Writer w] (.write w (str "Props:" x)))

(defrecord Evaluation [scores h-vals g-vals cutoff depth agent id]
  Object
  (toString [x] (str (select-keys x [:scores]))))
(defmethod print-method Evaluation [x ^java.io.Writer w] (.write w (str "Eval" x)))

(defn make-eval [score-vec h-vec g-vec cutoff maxmax-props agent]
  (Evaluation. score-vec h-vec g-vec cutoff (:depth maxmax-props) (str agent) (nano-id 5)))

(defn cutoff-maxmax [maxmax-props cutoff-depth]
  (log/debug "Depth cutoff!")
  (< cutoff-depth (:depth maxmax-props)))

(defn term-search? [{di-state :di-state agent-order :agent-order} maxmax-props]
  (or
    (both-agents-terminated di-state agent-order)
    (cutoff-maxmax maxmax-props (-> agent-order first :cutoff-depth))))

(defrecord M-O [op]
  Object
  (toString [x] (str (into {} x))))
(defmethod print-method M-O [x ^java.io.Writer w] (.write w (str "M-O:" x)))

(defn next-ops [{graph-desc :graph-desc di-state :di-state} op-agent]
  "[M-O1 M-O2 ...]"
  (let [n-ops (into [] (map #(M-O. %)
                            (gen-next-ops graph-desc di-state op-agent)))]
    n-ops))

(defrecord M-OGD [op graph-desc di-state]
  Object
  (toString [x] (str (into {} x))))
(defmethod print-method M-OGD [x ^java.io.Writer w] (.write w (str "M-OGD:" x)))
(defn make-m-ogd [m-o graph-desc di-state] (M-OGD. (:op m-o) graph-desc di-state))

(defrecord M-OGDMT [op graph-desc di-state evl tail]
  Object
  (toString [x] (str (into {} x))))
(defmethod print-method M-OGDMT [x ^java.io.Writer w] (.write w (str "M-OGDMT:" x)))
(defn make-m-ogdmt [m-ogd evl tail] (map->M-OGDMT (assoc m-ogd :evl evl :tail tail)))


(defn own-heuristic
  [{graph-desc :graph-desc di-state :di-state :as maxmax-state} agent]
  (let [other (gs/other-agent maxmax-state agent)]

    (agent-heuristic graph-desc di-state agent)

    ))

(defn stat-eval
  "return Evaluation of current state according to agent"
  [{di-state :di-state agent-order :agent-order heuristic :heuristic :as maxmax-state}
   maxmax-props
   agent]

  (let [agent-state (gs/state-of di-state agent)
        other-state (gs/other-agent-state maxmax-state agent)
        other-agent (gs/other-agent maxmax-state agent)

        g-score-self (calc-agent-score agent-state)
        g-score-other (calc-agent-score other-state)
        h-score-self (heuristic maxmax-state agent)
        h-score-other (heuristic maxmax-state other-agent)
        f-score-self (+ (* 2 g-score-self) (:val h-score-self))
        f-score-other (+ (* 2 g-score-other) (:val h-score-other))
        cutoff (not (both-agents-terminated di-state agent-order))]

    (make-eval [f-score-self f-score-other] [h-score-self h-score-other] [g-score-self g-score-other]
               cutoff, maxmax-props, agent)))

(defn assoc-op-with-res
  "Execute op on current graph and state
  returns:  M-OGD {:op Edge, :graph-dec GraphDescription :di-state TwoAgentState}"
  [graph-desc di-state agent m-o]

  (let [[new-graph-desc new-di-state] ((:op m-o) graph-desc di-state agent)]
    (make-m-ogd m-o new-graph-desc new-di-state)))

(defn compute-ops
  "Execute ops on current graph and state, returns a vector of results
  [M-OGD1 M-OGD2 ...]"
  [{graph-desc :graph-desc di-state :di-state} op-agent m-os]
  (let [m-ogds
        (into []
              (map #(assoc-op-with-res graph-desc di-state op-agent %) m-os))]
    m-ogds))

(defn assoc-res-with-max-val
  "input: op and its result - state & graph desc
  Recursively call max-value and let the other player play
  return: M-OGDMT"
  [{graph-desc :graph-desc di-state :di-state :as m-ogd} maxmax-state maxmax-props]

  (let [new-maxmax-state (update-graph-state maxmax-state graph-desc di-state)
        res-and-max-val (max-value (rev-order new-maxmax-state) maxmax-props)
        m-ogdmt (make-m-ogdmt m-ogd (:evl res-and-max-val) res-and-max-val)]

    m-ogdmt))

(defn current-maxifier [maxmax-state maxmax-props]
  (if (-> maxmax-state :agent-order first :name (= (-> maxmax-props :agent :name)))
    (:maxifier1 maxmax-props)
    (:maxifier2 maxmax-props)))

(defn inc-depth-calc-max-val
  "Update depth and associate with max-val recursively"
  [maxmax-state maxmax-props m-ogd]

  (let [incr-depth (inc-depth maxmax-state (:depth maxmax-props))
        m-ogdmt (assoc-res-with-max-val
                  m-ogd maxmax-state
                  (assoc maxmax-props :depth incr-depth))]
    m-ogdmt))

(defn calc-m-ogdmt-vec
  "Calculate max scores for op results"
  [maxmax-state maxmax-props m-ogd-vec]
  (into [] (map #(inc-depth-calc-max-val maxmax-state maxmax-props %) m-ogd-vec)))

(defn max-m-ogdmt
  "Calculate max scores for op results and return the maximal"
  [maxmax-state maxmax-props m-ogd-vec]

  (let [maxifier (current-maxifier maxmax-state maxmax-props)
        m-ogdmt-vec (calc-m-ogdmt-vec maxmax-state maxmax-props m-ogd-vec)]
    (last (sort-by maxifier m-ogdmt-vec))))

(defn max-value
  "From a state - run all possible ops recursively
  and return the one with the maximal score
  {:val Evaluation :tail {<previous op> :op :di-state ... :tail}}]}"
  [maxmax-state maxmax-props]

  (let [[agent op-agent] [(:agent maxmax-props) (first (:agent-order maxmax-state))]]

    (if (term-search? maxmax-state maxmax-props)
      (let [evl (stat-eval maxmax-state maxmax-props agent)]
        {:evl evl :tail nil})

      (let [ops (next-ops maxmax-state op-agent)
            ops-and-results (compute-ops maxmax-state op-agent ops)
            ops-and-res-tick (progress-tick maxmax-props op-agent ops-and-results)
            maximal-m-ogdmt (max-m-ogdmt maxmax-state maxmax-props ops-and-res-tick)]

        maximal-m-ogdmt))))




(defn max-max [graph-desc di-state agent-order]

  (log/debug (str (first agent-order)) ">>> " (:remaining-people graph-desc))

  (let [maxifier1 (:maxifier1 (first agent-order))
        maxifier2 (:maxifier2 (first agent-order))
        time-progressor (first (filter #(= (:name %) "Bob") agent-order))
        initial-maxmax-state (MaxMaxState. graph-desc di-state own-heuristic agent-order)
        initial-maxmax-props (MaxMaxNodeProps. 0 maxifier1 maxifier2 time-progressor (first agent-order))
        op-agent (first agent-order)]

    (let [m-o-vec (next-ops initial-maxmax-state op-agent)
          m-ogd-vec (compute-ops initial-maxmax-state op-agent m-o-vec)
          m-ogdmt-vec (into [] (map #(assoc-res-with-max-val % initial-maxmax-state initial-maxmax-props) m-ogd-vec))]

      (doseq [m m-ogdmt-vec]
        (log/debug (str (first agent-order)) " &>>" (:evl m) (into {} (:op m)) (tail-summary-str m)))

      (:op
        (last (sort-by maxifier1 m-ogdmt-vec))))))