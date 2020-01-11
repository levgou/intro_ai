(ns introai.assignment3.bayes-network
  (:gen-class)
  (:require
    [loom.graph :as graph]
    [loom.alg :as g-alg]
    [introai.utils.log :as log]
    [clojure.core.strint :refer [<<]]
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

(def NO_FLOOD_BLOCK_PROBA 0.001)
(def ONE_FLOOD_BLOCK_PROBA 0.6)
(def TWO_FLOOD_BLOCK_PROBA 0.84)

(def LINE_SEP (clojure.string/join "" (repeat 80 "=")))

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

(defrecord BayesNet [structure t persistence])

(defn dec-t [vertex] (update vertex :t - 1))

(defn prev-time-flood-proba
  [b-net vertex persistence]
  (*
    persistence
    (look-up-flood b-net (dec-t vertex) persistence)))

(defn prev-time-no-flood-proba
  [b-net vertex persistence]
  (*
    (:proba vertex)
    (- 1 (look-up-flood b-net (dec-t vertex) persistence))))

(defn look-up-flood
  [b-net vertex persistence]
  (if (zero? (:t vertex))
    (:proba vertex)
    (+
      (prev-time-flood-proba b-net vertex persistence)
      (prev-time-no-flood-proba b-net vertex persistence)
      )))

;(defn look-up-block
;  [b-net edge-node src-floods dest-floods persistence]
;  (if (zero? (:t edge-node)))
;
;  (let [[u v] (graph/predecessors b-net edge-node)]
;    0))

(defn edges-for-edge-node
  [v-name_t->vertex {:keys [src dest t] :as edge-node}]
  [[(v-name_t->vertex [src t]) edge-node], [(v-name_t->vertex [dest t]) edge-node]])

(defn gen-b-edges
  [v-name_t->vertex edge-nodes]
  (->> edge-nodes
       (map #(edges-for-edge-node v-name_t->vertex %))
       (mapcat identity)))

(defn map-name->vertex
  [vertex-nodes]
  (zipmap (map (juxt :name :t) vertex-nodes) vertex-nodes))

(defn extract-init-probas
  [{{node-infos :nodes} :props}]
  (zipmap (keys node-infos)
          (map :flood-prob (vals node-infos))))

(defn gen-b-net-t-0
  [{g-struct :structure :as g-desc} persistence]
  (let [initial-flood-proba-map (extract-init-probas g-desc)
        g-edges (g-alg/distinct-edges g-struct)
        g-nodes (graph/nodes g-struct)
        edge-nodes (map #(gen-edge-node (first %) (second %)) g-edges)
        vertex-nodes (map #(time-zero-vertex-node % initial-flood-proba-map) g-nodes)
        name_t->vertex (map-name->vertex vertex-nodes)
        b-edges (gen-b-edges name_t->vertex edge-nodes)
        b-net-struct (apply graph/digraph b-edges)]

    (BayesNet. b-net-struct 0 persistence)))


(defn filter-vertices
  [b-struct]
  (filter :proba (graph/nodes b-struct)))

(defn filter-edges
  [b-struct]
  (remove :proba (graph/nodes b-struct)))

(defn filter-nodes-t
  [b-net node-type-filter t]
  (->> b-net
       :structure
       node-type-filter
       (filter #(= t (:t %)))))

(defn filter-vertices-t
  [b-net t]
  (filter-nodes-t b-net filter-vertices t))

(defn filter-edges-t
  [b-net t]
  (filter-nodes-t b-net filter-edges t))

(defn gen-new-b-vertex-nodes
  [{structure :structure t :t}]
  (let [vertex-nodes (filter-vertices structure)
        newest-vertex-nodes (filter #(= t (:t %)) vertex-nodes)
        new-nodes (map #(update % :t inc) newest-vertex-nodes)]
    (concat vertex-nodes new-nodes)))

(defn gen-new-b-edge-nodes
  [{structure :structure t :t}]
  (let [edge-nodes (filter-edges structure)
        new-nodes (map #(update % :t inc) (filter #(= t (:t %)) edge-nodes))]
    (concat edge-nodes new-nodes)))

(defn gen-vertex-vertex-edges
  [new-b-vertex-nodes]
  (let [t>0-vertices (filter #(< 0 (:t %)) new-b-vertex-nodes)
        t-1-vertices (map #(update % :t dec) t>0-vertices)
        edge-map (zipmap t-1-vertices t>0-vertices)]

    (into [] edge-map)))

(defn progress-t-b-net
  [{persistence :persistence t :t :as b-net}]
  (let [t+1 (inc t)
        new-b-vertex-nodes (gen-new-b-vertex-nodes b-net)
        new-b-edge-nodes (gen-new-b-edge-nodes b-net)
        name_t->vertex (map-name->vertex new-b-vertex-nodes)
        edge-vertex-edges (gen-b-edges name_t->vertex new-b-edge-nodes)
        vertex-vertex-edges (gen-vertex-vertex-edges new-b-vertex-nodes)
        all-new-edges (concat vertex-vertex-edges edge-vertex-edges)
        b-net-struct (apply graph/digraph all-new-edges)
        ]
    (BayesNet. b-net-struct t+1 persistence)))

(defn b-net-with-t
  [b-net t]
  (last (take (inc t) (iterate progress-t-b-net b-net))))


(defn print-vertex
  [{persistence :persistence :as b-net} {:keys [proba name t] :as vertex-node}]
  (let [p-not-flood (- 1 proba)]

    (println "")
    (println "VERTEX" name "time" t ":")

    (if (= 0 t)
      (do
        (println "  P(Flooding)      = " proba)
        (println "  P(not Flooding)  = " p-not-flood))
      (do
        (println (<< "  P(Flooding |     Flooding t=~{(- t 1)})  =  ~{persistence}"))
        (println (<< "  P(Flooding | not Flooding t=~{(- t 1)})  =  ~{proba}"))

        ))))

(defn print-edge
  [b-net {:keys [src dest t]}]

  (println "")
  (println "EDGE" src "->" dest "time" t ":")
  (println (<< "  P(Block | no flood ~{src}, no flood ~{dest} [t=~{t}]) = ") NO_FLOOD_BLOCK_PROBA)
  (println (<< "  P(Block |    flood ~{src}, no flood ~{dest} [t=~{t}]) = ") ONE_FLOOD_BLOCK_PROBA)
  (println (<< "  P(Block | no flood ~{src},    flood ~{dest} [t=~{t}]) = ") ONE_FLOOD_BLOCK_PROBA)
  (println (<< "  P(Block |    flood ~{src},    flood ~{dest} [t=~{t}]) = ") TWO_FLOOD_BLOCK_PROBA))

(defn print-b-net-t-vertices
  [b-net t]
  (let [vertex-nodes-t (filter-vertices-t b-net t)]
    (doseq [vertex vertex-nodes-t] (print-vertex b-net vertex))))

(defn print-b-net-t-edges
  [b-net t]
  (let [edge-nodes (filter-edges-t b-net t)]
    (doseq [edge edge-nodes] (print-edge b-net edge))))

(defn print-b-net-t
  [b-net t]
  (print-b-net-t-vertices b-net t)
  (print-b-net-t-edges b-net t)
  (println LINE_SEP))


(defn print-b-net
  [b-net]
  (print-b-net-t b-net 0)
  (print-b-net-t b-net 1))

(defn proba-lut
  [b-node parents-assign persistence]

  (if (instance? VertexNode b-node)

    ; VERTEX
    (if (first parents-assign)
      persistence
      (:proba b-node))

    ; EDGE
    (let [parents-floods-count (count (filter true? parents-assign))]
      (case parents-floods-count
        2 TWO_FLOOD_BLOCK_PROBA
        1 ONE_FLOOD_BLOCK_PROBA
        0 NO_FLOOD_BLOCK_PROBA))))
