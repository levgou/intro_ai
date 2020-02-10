(ns introai.assignment4.belief-space
  (:gen-class)
  (:require
    [introai.utils.const :as E]
    [introai.assignment4.game-state :as gs]
    [introai.assignment4.graph-description :as gd]
    [nano-id.core :refer [nano-id]]
    [introai.utils.graphs :as gutils]
    [clojure.core.strint :refer [<<]]
    [loom.graph :as graph]
    [loom.alg :refer [topsort]]
    ))


; Belief space structure
;
; Belief: g-desc, state
; Parent: Other belief
; proba: probability to trans to this belief from parent
; children: {
;   term: {1: belief}
;   edge: {0.3: one-belief, 0.7: other-belief{
; }
;

(def SHOW-CHILDREN false)


(defrecord Belief [parent g-desc state children proba uncertain op]
  Object
  (toString [x] (str
                  (->
                    (into {}
                          (sort-by #(if (= :children (first %)) 1 0)
                                   (assoc x :parent (-> x :parent :state :id)))
                          )

                    (select-keys (if SHOW-CHILDREN [:op :children] [:op]))
                    ))))

(defmethod print-method Belief [x ^java.io.Writer w] (.write w (str x)))

(defn certain? [belief] (not (:uncertain belief)))


(defn belief-term-score [belief]
  (if-not (-> belief :state gs/term?)
    (throw (Exception. "Wot ?"))
    (- (-> belief :state :saved)
       ({E/TERMINATED-SAFELY 0 E/TERMINATED-UNSAFELY 0.1} (-> belief :state :terminated)))))


(declare resolve-belief arrange-parent-child)
(defn init-belief [g-desc]
  (let [state (gs/init-state (-> g-desc :props :start) (:init-people g-desc))
        init-b (Belief. nil g-desc state [] 1 true (str "INIT" (:agent-node state)))
        resolved-edges-init-b {init-b (resolve-belief init-b)}
        pointing-beliefs (arrange-parent-child resolved-edges-init-b)]

    (first pointing-beliefs)))


(defn block-edge [edge] (assoc edge :blocked E/BLOCKED-TRUE))
(defn unblock-edge [edge] (assoc edge :blocked E/BLOCKED-FALSE))


(defn resolve-rest [proba resolved [edge & others]]
  (if (nil? edge)
    [[proba resolved]]

    (let [updated-proba-block (* proba (:block-proba edge))
          updated-proba-unblock (* proba (- 1 (:block-proba edge)))]

      (concat (resolve-rest updated-proba-block (conj resolved (block-edge edge)) others)
              (resolve-rest updated-proba-unblock (conj resolved (unblock-edge edge)) others)))))


(defn resolve-belief-possibility [belief [proba edges]]
  (let [edges-map (into {} (map #(vector (:name %) %) edges))]

    (-> belief
        (update-in [:g-desc :props :edges] merge edges-map)
        (assoc :proba proba :op (<< "~{(:op belief)}->P(~(format \"%.5f\" proba))}")))))


(defn resolve-edges-proba [unresolved-edges belief]
  (let [possibilities (resolve-rest 1 [] unresolved-edges)
        resolved-beliefs (map #(resolve-belief-possibility belief %) possibilities)]
    resolved-beliefs))


(defn resolve-belief [belief]
  (let [agent-node (-> belief :state :agent-node)
        edges (-> belief :g-desc :props :edges vals)

        resolved? #(= E/BLOCKED-UNKNOWN (:blocked %))
        near-agent? #(or (= agent-node (:start %)) (= agent-node (:end %)))

        unresolved-edges (filter (every-pred near-agent? resolved?) edges)
        resolved-beliefs (if (empty? unresolved-edges)
                           [(assoc belief :proba 1 :uncertain false)]
                           (resolve-edges-proba unresolved-edges belief))]
    resolved-beliefs))


(defn term-type-of [graph-desc state]
  (if (and (gd/shelter? graph-desc state)
           (not (gd/time-over? graph-desc state)))
    E/TERMINATED-SAFELY
    E/TERMINATED-UNSAFELY))


(defn update-dead-count [{dead-map :dead :as state}]
  (-> dead-map
      (update E/DIED-IN-CITY + (gd/all-people state))
      (update E/DIED-WITH-AGENT + (:carrying state))))


(defn term
  [{g-desc :g-desc state :state :as belief}]
  (let [new-state (assoc state
                    :terminated (term-type-of g-desc state)
                    :dead (update-dead-count state)
                    :carrying 0
                    :id (nano-id 7))]
    (assoc belief :state new-state :uncertain false :op (<< "~{(:op belief)}->TERM"))))


(defn put-people-shelter [g-desc state]
  (if-not (gd/shelter? g-desc state)
    state
    (assoc (update state :saved + (:carrying state)) :carrying 0)))


(defn pick-up-people [state]
  (assoc state
    :carrying (+ (:carrying state)
                 (gd/people-num state))
    :remaining-people (gs/rem-people state)
    :id (nano-id 7)))


(defn traverse-to
  [{g-desc :g-desc {agent-node :agent-node :as state} :state :as belief} dest]
  (let [edge (gutils/relevant-edge g-desc state dest)
        new-state (->> state (gs/progress-time edge) (gs/traverse-edge edge))
        ops-str (<< "~{(:op belief)}->GO[~{agent-node},~{dest}]")]

    (if (gd/time-over? g-desc new-state)
      (term (assoc belief :state new-state :op ops-str))

      (->> new-state
           (put-people-shelter g-desc)
           pick-up-people
           (assoc belief :op ops-str :state)))))


(defn- set-parent-children [parent children]
  (let [fathered-children (into [] (map #(assoc % :parent parent) children))]
    (assoc parent :children fathered-children)))

(defn- arrange-parent-child [resolved-edges]
  (map

    (fn [[edge-uncertain children]]
      (let [certain-children (into [] (map #(assoc % :uncertain false) children))]

        (if (= 1 (count certain-children))
          (first certain-children)
          (let [pointing-parent (set-parent-children edge-uncertain certain-children)]
            (assoc pointing-parent :uncertain true)))))

    resolved-edges))


(defn next-beliefs [{g-desc :g-desc state :state :as belief}]
  (let [term-belief (term belief)
        edge-beliefs-no-proba (->> (gutils/state-unblocked-successors g-desc state)
                                   (map #(traverse-to belief %)))
        resolved-edges (apply merge (into [] (map #(hash-map % (resolve-belief %)) edge-beliefs-no-proba)))
        pointing-resolved-edges (into [] (arrange-parent-child resolved-edges))]

    (conj pointing-resolved-edges term-belief)))


(declare expand-belief)

(defn expand-certain-children [uncertain-belief]
  (let [children (:children uncertain-belief)
        expanded-children (into [] (map expand-belief children))]

    (set-parent-children uncertain-belief expanded-children)))


(def expander #(if (certain? %) (expand-belief %) (expand-certain-children %)))

(defn expand-belief [belief]
  (if (-> belief :state gs/term?)
    belief

    (let [next-bs (next-beliefs belief)
          expanded-next-bs (into [] (map expander next-bs))]

      (set-parent-children belief expanded-next-bs))))

(defn expand-init-belief [init-b]
  (if (certain? init-b)
    (expand-belief init-b)
    (expand-certain-children init-b)))


(defn belief-tree-di-edges [{children :children :as belief}]
  (if (empty? children)
    []

    (let [cur-edges (map #(vector % belief) children)
          childrens-edges (apply concat (map belief-tree-di-edges children))]
      (concat cur-edges childrens-edges))))


(defn belief-tree-to-graph [belief]
  (let [edges (belief-tree-di-edges belief)
        g (apply graph/digraph edges)]
    g))


(defn calc-belief-score [b-graph calced-map belief]
  (if (-> belief :state gs/term?)
    (belief-term-score belief)

    (let [predecessors (graph/predecessors b-graph belief)]
      (if (certain? belief)
        (apply max (map calced-map predecessors))
        (apply + (map #(* (:proba %) (calced-map %)) predecessors))))))


(defn calc-scores [b-graph top-order]
  (reduce
    (fn [calced belief]
      (assoc calced belief (calc-belief-score b-graph calced belief)))
    {}
    top-order))


(defn scored-graph [belief]
  (let [bg (belief-tree-to-graph belief)
        top-order (topsort bg)
        score-index (calc-scores bg top-order)]
    {:b-graph bg :score-index score-index :start-belief (last top-order)}))