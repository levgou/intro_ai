(ns introai.assignment1.graph-description
  (:gen-class))

(defrecord NodeInfo [name dead-line num-persons has-shelter])

(defrecord EdgeInfo [name start end weight])

(defrecord GraphProps [num-nodes shelters nodes edges time-modifier])

(defrecord GraphDescription [structure props])

(defn state-node-info [graph-desc state]
  (-> graph-desc :props :nodes (#(% (:agent-node state)))))

(defn state-node-info-piece [graph-desc state key]
  (let [state-node (state-node-info graph-desc state)]
    (key state-node)))

(defn time-over? [graph-desc state]
  (let [deadline (+ (state-node-info-piece graph-desc state :dead-line) (-> graph-desc :props :time-modifier))]
    (> (:time state) deadline)))

(defn shelter? [graph-desc state]
  (state-node-info-piece graph-desc state :has-shelter))

(defn people-num [graph-desc state]
  (state-node-info-piece graph-desc state :num-persons))

(defn non-empty-nodes [graph-desc]
  (remove #(zero? (:num-persons %)) (-> graph-desc :props :nodes vals)))

(defn people-map [graph-desc]
  (let [non-zero-nodes (non-empty-nodes graph-desc)]
    (apply hash-map
           (apply concat
                  (map #(vals (select-keys % [:name :num-persons])) non-zero-nodes)))))

(defn remaining-nodes [{ppl-map :remaining-people}]
  (keys ppl-map))

(defn all-people [state]
  (->> state :remaining-people vals (apply +)))
