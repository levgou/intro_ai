(ns introai.assignment1.agents.tree-search)

(defn traverse-tree
  [fringe goal? expand]

  (loop [[min-node & others] fringe]
    (if (nil? min-node)
      nil
      (if (goal? min-node)
        min-node
        )))

  (if (empty? fringe)
    nil
    (let [[min-node & others] fringe]
      (if (goal? min-node)
        min-node
        )))
  )


(defn main-loop [graph-desc init-state choose-op]
  (loop [state init-state iteration 0]
    (log/info (<< "(~{iteration})> ~{(into {} state)}"))
    (if (:terminated state)
      state
      (recur
        ((choose-op graph-desc state) state)
        (inc iteration)))))

(defn tree-search
  [init-state fringe goal-test expand]
  (let [initial-fringe (-> init-state make-node (#(conj fringe %)))]
    (traverse-tree initial-fringe goal-test expand)))