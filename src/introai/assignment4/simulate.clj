(ns introai.assignment4.simulate
  (:gen-class)
  (:require
    [clojure.pprint :refer [pprint]]
    [introai.assignment4.read-graph :as rg]
    [introai.assignment4.belief-space :as bs]
    [loom.graph :as graph]
    [introai.assignment4.game-state :as gs]
    [introai.utils.graphs :as gutils]))


(defn wrand
  "given a vector of slice sizes, returns the index of a slice given a
  random spin of a roulette wheel with compartments proportional to
  slices."
  [slices]
  (let [total (reduce + slices)
        r (rand total)]
    (loop [i 0 sum 0]
      (if (< r (+ (slices i) sum))
        i
        (recur (inc i) (+ (slices i) sum))))))


(defn wrand-belief [beliefs]
  (nth beliefs (wrand (into [] (map :proba beliefs)))))


(defn just-resolved-edges [uncertain-b res-b]
  (let [cur-node (-> uncertain-b :state :agent-node)

        uncertain-edges (-> uncertain-b
                            :g-desc
                            :props
                            :edges
                            (gutils/edge-names-at-node cur-node))

        resolved-state (map #(select-keys ((-> res-b :g-desc :props :edges) %) [:start :end :blocked])
                            uncertain-edges)]

    resolved-state))


(defn b-term? [belief] (-> belief :state gs/term?))


(defn max-next-belief [b-graph score-index belief]
  (->> belief
       (graph/predecessors b-graph)
       (map #(vector % (score-index %)))
       (apply max-key second)
       (#(do (println "[C]hoooose: " (second %) "-" (first %)) (first %)))))


(defn resolve-uncertainty [b-graph belief]
  (if (bs/certain? belief)
    belief
    (->> belief
         (graph/predecessors b-graph)
         (into [])
         wrand-belief
         (#(do (println "[R]esolved: " % (just-resolved-edges belief %)) %)))))


(defn main-loop [b-graph score-index start-belief]

  (let [choose-act #(max-next-belief b-graph score-index %)
        resolve-b #(resolve-uncertainty b-graph %)]

    (loop [cur-b start-belief]
      (if (b-term? cur-b)
        cur-b
        (let [resolved-b (resolve-b cur-b)
              next-b (choose-act resolved-b)]

          (recur next-b))))))


(defn run-simu [file-path]
  (let
    [G (rg/read-graph-from-file file-path)
     B (bs/init-belief G)
     BX (bs/expand-init-belief B)
     {:keys [b-graph score-index start-belief]} (bs/scored-graph BX)

     _ (println "[F]iiiirst: " (score-index start-belief) "-" start-belief "\n")
     final-b (main-loop b-graph score-index start-belief)]))
