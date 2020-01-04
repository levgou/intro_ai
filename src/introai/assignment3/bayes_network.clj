(ns introai.assignment3.bayes-network
  (:gen-class)
  (:require
    [loom.graph :as graph]
    [loom.alg :as g-alg]
    ))

;;  Fl(v,t) standing in for "flooding" at vertex v at time t,
;;  B(e,t) standing in for "blocked" for each edge e at time t
;;
;;  for e = (u, v) and some t:
;;    P(B(e,t) | not Fl(v,t)  and  not Fl(u,t))  =  0.001
;;    P(B(e,t) |     Fl(v,t)  and  not Fl(u,t))  =  0.6
;;    P(B(e,t) |     Fl(u,t)  and  not Fl(v,t))  =  0.6
;;    P(B(e,t) |     Fl(u,t)  and      Fl(v,t))  =  0.84
;;
;;  P(Fl(v,t+1) |     Fl(v,t)) = Persistence
;;  P(Fl(v,t+1) | not Fl(v,t)) = P(Fl(v,0))
;;
;;    Thus:
;;      P(Fl(v,t+1)) = [Persistence * P(Fl(v,t))] + [P(Fl(v,0)) * P(not Fl(v,t))]

(declare look-up-flood)

(defrecord VertexNode [name proba t]
  Object
  (toString [vn] (str "VertexNode: " (into {} vn))))
(defmethod print-method VertexNode [gs ^java.io.Writer w] (.write w (str gs)))

(defn time-zero-vertex-node
  [name initial-flood-proba-map]
  (VertexNode. name (initial-flood-proba-map name) 0))

(defrecord EdgeNode [src dest t]
  Object
  (toString [en] (str "EdgeNode: " (into {} en))))
(defmethod print-method EdgeNode [gs ^java.io.Writer w] (.write w (str gs)))

(defn gen-edge-node [u v] (apply ->EdgeNode (concat (sort [u v]) [0])))

(defrecord BayesNet [structure t])

(defn prev-time-flood-proba
  [b-net vertex persistence]
  (*
    persistence
    (look-up-flood b-net (update vertex :t - 1) persistence)))

(defn prev-time-no-flood-proba
  [b-net vertex persistence]
  (*
    (:proba vertex)
    (- 1 (look-up-flood b-net vertex persistence))))

(defn look-up-flood
  [b-net vertex persistence]
  (if (zero? (:t vertex))
    (:proba vertex)
    (+
      (prev-time-flood-proba b-net vertex persistence)
      (prev-time-no-flood-proba b-net vertex persistence)
      )))

(defn look-up-block
  [b-net vertex persistence]
  (let [[u v] (graph/predecessors b-net vertex)]
    ))

(defn gen-b-edges
  [v-name->vertex edge-nodes]
  (->> edge-nodes
       (map #(vector [% (v-name->vertex (:src %))] [% (v-name->vertex (:dest %))]))
       (mapcat identity)))

(defn map-name->vertex
  [vertex-nodes]
  (zipmap (map :name vertex-nodes) vertex-nodes))

(defn gen-bayes-net
  [edge-nodes vertex-nodes t]
  (let [name->vertex (map-name->vertex vertex-nodes)
        b-edges (gen-b-edges name->vertex edge-nodes)
        b-net-struct (apply graph/digraph b-edges)]

    (BayesNet. b-net-struct t)))

(defn gen-b-net-t-0
  [g-struct initial-flood-proba-map]
  (let [g-edges (g-alg/distinct-edges g-struct)
        g-nodes (graph/nodes g-struct)
        edge-nodes (map #(gen-edge-node (first %) (second %)) g-edges)
        vertex-nodes (map #(time-zero-vertex-node % initial-flood-proba-map) g-nodes)
        name->vertex (map-name->vertex vertex-nodes)
        b-edges (gen-b-edges name->vertex edge-nodes)
        b-net-struct (apply graph/digraph b-edges)
        ]

    (BayesNet. b-net-struct 0)))


(defn filter-vertices
  [b-struct]
  (filter :proba (graph/nodes b-struct)))

(defn filter-edges
  [b-struct]
  (remove :proba (graph/nodes b-struct)))

(defn gen-new-b-vertex-nodes
  [{structure :structure}]
  (let [vertex-nodes (filter-vertices structure)
        new-nodes (map #(update % :t inc) vertex-nodes)]
    new-nodes))

(defn gen-new-b-edge-nodes
  [{structure :structure}]
  (let [edge-nodes (filter-edges structure)
        new-nodes (map #(update % :t inc) edge-nodes)]
    new-nodes))

(defn gen-vertex-vertex-edges
  [new-b-vertex-nodes]
  (into []
        (zipmap new-b-vertex-nodes
                (map #(update % :t dec) new-b-vertex-nodes))))

(defn progress-t-b-net
  [b-net]
  (let [new-t (inc (:t b-net))
        new-b-vertex-nodes (gen-new-b-vertex-nodes b-net)
        new-b-edge-nodes (gen-new-b-edge-nodes b-net)
        name->vertex (map-name->vertex new-b-vertex-nodes)
        edge-vertex-edges (gen-b-edges name->vertex new-b-edge-nodes)
        vertex-vertex-edges (gen-vertex-vertex-edges new-b-vertex-nodes)
        all-new-edges (concat vertex-vertex-edges edge-vertex-edges)
        b-net-struct (apply graph/digraph all-new-edges)
        ]
    (gen-bayes-net new-b-edge-nodes new-b-vertex-nodes new-t)))