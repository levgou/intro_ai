(ns introai.assignment2.agents.mini-max
  (:gen-class)
  (:require
    [introai.assignment2.agents.game-funcs :refer [gen-next-ops agent-term?]]
    [introai.assignment2.game-state :as gs]
    [introai.utils.log :as log]
    [introai.utils.enums :as E]))


(declare min-value)
(declare max-value)
(declare assoc-res-with-min-val)
(declare assoc-res-with-max-val)
(declare assoc-op-with-res)

(def INF ##Inf)
(def -INF (- ##Inf))

(defn term-search? [di-state agent-order]
  (and
    (agent-term? di-state (first agent-order))
    (agent-term? di-state (second agent-order))
    ))

(defn calc-agent-score [state]
  (-
    (:saved state)

    (if (= (:terminated state) E/TERMINATED-UNSAFELY) 2 0)
    (-> state :dead E/DIED-IN-CITY)
    (* 2 (-> state :dead E/DIED-WITH-AGENT))
    ))


(defn stat-eval [di-state agent]
  (calc-agent-score (gs/state-of di-state agent)))

(defn assoc-op-with-res [graph-desc di-state agent map-with-op]
  (let [[new-graph-desc new-di-state] ((:op map-with-op) graph-desc di-state agent)]
    (assoc map-with-op :graph-desc new-graph-desc :di-state new-di-state)))

(defn assoc-res-with-min-val
  [{op :op graph-desc :graph-desc di-state :di-state :as m} agent-order alpha beta]
  (let [min-val (min-value graph-desc di-state agent-order alpha beta)]
    (assoc m :min-val min-val)))

(defn assoc-res-with-max-val
  [{op :op graph-desc :graph-desc di-state :di-state :as m} agent-order alpha beta]
  (let [max-val (max-value graph-desc di-state agent-order alpha beta)]
    (assoc m :max-val max-val)))

(defn prune-next-states-max [agent-order results alpha beta]
  (loop [[res & others] results
         max-val -INF
         cur-alpha alpha]

    (let [v (max max-val (:min-val (assoc-res-with-min-val res agent-order cur-alpha beta)))]

      (if (or (>= v beta) (nil? others))
        v
        (recur others v (max cur-alpha v))))))

(defn max-value
  [graph-desc di-state agent-order alpha beta]

  (let [[agent op-agent] [(first agent-order) (first agent-order)]]

    (if (term-search? di-state agent-order)
      (let [score (stat-eval di-state agent)]
        ;(log/info "MAX LEAF! - " score "  -  " (into {} (gs/state-of di-state agent)))
        score)

      (let [ops (map #(identity {:op %}) (gen-next-ops graph-desc di-state op-agent))]
        (let [ops-and-results (map #(assoc-op-with-res graph-desc di-state op-agent %) ops)]
          (prune-next-states-max agent-order ops-and-results alpha beta)



          ;(let [maps-with-min-vals (into [] (map #(assoc-res-with-min-val % agent-order) ops-and-results))]

          ;(log/info ">>> ######## max-value ###########################################################################")
          ;(doseq [m maps-with-min-vals]
          ;  (log/info (:min-val m) (into {} (:op m))))
          ;(log/info "<<< ######## max-value ###########################################################################")


          ;(let [min-vals (into [] (map :min-val maps-with-min-vals))]
          ;  (apply max min-vals)))
          ;

          )))))

(defn prune-next-states-min [agent-order results alpha beta]
  (loop [[res & others] results
         min-val INF
         cur-beta beta]

    (let [v (min min-val (:max-val (assoc-res-with-max-val res agent-order alpha cur-beta)))]

      (if (or (<= v alpha) (nil? others))
        v
        (recur others v (min cur-beta v))))))

(defn min-value
  "The other player plays ->
   will try to choose the min for the first player"
  [graph-desc di-state agent-order alpha beta]

  (let [[agent op-agent] agent-order]

    (if (term-search? di-state agent-order)
      (let [score (stat-eval di-state agent)]
        ;(log/info "MIN LEAF! - " score "  -  " (into {} (gs/state-of di-state agent)))
        score)

      (let [ops (into [] (map #(identity {:op %}) (gen-next-ops graph-desc di-state op-agent)))]
        (let [ops-and-results (into [] (map #(assoc-op-with-res graph-desc di-state op-agent %) ops))]

          (prune-next-states-min agent-order ops-and-results alpha beta)
          ;(let [maps-with-max-vals (into [] (map #(assoc-res-with-max-val % agent-order) ops-and-results))]

          ;(log/info ">>> $$$ min-value $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$")
          ;(doseq [m maps-with-max-vals]
          ;  (log/info (:max-val m) (into {} (:op m))))
          ;(log/info "<<< $$$ min-value $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$")


          ;(let [max-vals (into [] (map :max-val maps-with-max-vals))]
          ;  (apply min max-vals)))
          ;
          )))))


(defn mini-max [graph-desc di-state agent agent-order]
  (let [ops (map #(identity {:op %}) (gen-next-ops graph-desc di-state agent))]
    (let [ops-and-results (map #(assoc-op-with-res graph-desc di-state agent %) ops)]
      (let [maps-with-min-vals (into [] (map #(assoc-res-with-min-val % agent-order -INF INF) ops-and-results))]

        (doseq [m maps-with-min-vals]
          (log/debug "&>>" (:min-val m) (into {} (:op m))))

        (:op (apply max-key :min-val maps-with-min-vals))))))