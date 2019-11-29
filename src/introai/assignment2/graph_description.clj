(ns introai.assignment2.graph-description
  (:gen-class))

(defrecord NodeInfo [name dead-line num-persons has-shelter])

(defrecord EdgeInfo [name start end weight])

(defrecord GraphProps [num-nodes shelters nodes edges time-modifier])

(defrecord GraphDescription [structure props remaining-people])

(defn state-node-info [graph-desc state]
  (-> graph-desc :props :nodes (#(% (:agent-node state)))))

(defn state-node-info-piece [graph-desc state key]
  (let [state-node (state-node-info graph-desc state)]
    (key state-node)))

(defn time-over? [graph-desc state cur-time]
  (let [deadline (+ (state-node-info-piece graph-desc state :dead-line)
                    (-> graph-desc :props :time-modifier))]
    (> cur-time deadline)))

(defn shelter? [graph-desc state]
  (state-node-info-piece graph-desc state :has-shelter))

(defn people-num [graph-desc state]
  (or
    ((:remaining-people graph-desc) (:agent-node state))
    0)
  ;(state-node-info-piece graph-desc state :num-persons)
  )

(defn non-empty-nodes [nodes-map]
  (remove #(zero? (:num-persons %)) (vals nodes-map)))

(defn people-map [nodes-map]
  (let [non-zero-nodes (non-empty-nodes nodes-map)]
    (apply hash-map
           (apply concat
                  (map #(vals (select-keys % [:name :num-persons])) non-zero-nodes)))))

(defn remaining-nodes [{ppl-map :remaining-people}]
  (keys ppl-map))

(defn all-people [graph-desc]
  (->> graph-desc :remaining-people vals (apply +)))
