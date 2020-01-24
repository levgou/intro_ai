(ns introai.assignment4.rand-sample
  (:gen-class)
  (:require
    [loom.alg :refer [topsort]]
    [loom.graph :refer [predecessors]]
    [introai.assignment4.bayes-network :as bn]
    [introai.utils.collections :refer [maps-agree]]
    [introai.utils.log :as log]

    ))

(defn assign-b-node
  [b-struct b-node assign persistence]
  (let [parents (predecessors b-struct b-node)
        parent-status (map assign parents)
        t-prob (bn/proba-lut b-node parent-status persistence)
        node-assign (<= (rand) t-prob)]

    ;(println assign )
    ;(println b-node t-prob node-assign)
    ;(println "$$$$$$$$$$$$$$$")
    node-assign))

(defn sample-bayes
  [topological b-struct persistence]

  (loop [[b-node & rest] topological
         assign {}]

    (let [node-assign (assign-b-node b-struct b-node assign persistence)
          new-assign (assoc assign b-node node-assign)]
      (if (nil? rest)
        new-assign
        (recur rest new-assign)))))

(defn inf-sample [bayes]
  (let [{b-struct :structure persistence :persistence} bayes
        topological (topsort b-struct)]
    (repeatedly #(sample-bayes topological b-struct persistence))))

(defn vals-bool->int [m]
  (zipmap (keys m) (map {true 1 false 0} (vals m))))

(defn count-true-for-keys [ms]
  (->> ms
      (map vals-bool->int)
      (apply merge-with +)))

(defn sampled-dist
  [bayes query-vars evidence-map sample-size]
  (->> (inf-sample bayes)
       (filter #(maps-agree % evidence-map))
       (take sample-size)
       (map #(select-keys % query-vars))

       count-true-for-keys))

(defn sampled-proba
  "Evidence map is mapping between bayes nodes to a bool value"
  [bayes query-vars evidence-map sample-size]
  (let [dists (sampled-dist bayes query-vars evidence-map sample-size)]
    (into {}
          (map #(vector % (float (/ (dists %) sample-size))) query-vars))))