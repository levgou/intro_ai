(ns introai.assignment3.main-loop
  (:gen-class)
  (:require
    [clojure.core.strint :refer [<<]]
    [introai.assignment3.bayes-network :as bn]
    [introai.assignment3.rand-sample :refer [sampled-proba]]
    [clojure.string :as str]
    [introai.utils.collections :refer [pairwise-collection]]
    ))

(def EVIDENCE_OPTIONS
  {
   1 :flood
   2 :no-flood
   3 :block
   4 :no-block
   })

(def EDGE_OR_VERTEX
  {
   1 "Vertex"
   2 "Vertex"
   3 "Edge"
   4 "Edge"
   })

(def ACTIONS
  {
   "r" "Reset evidence list to empty"
   "a" "Add piece of evidence to evidence list"
   "l" "Print evidence list"
   "p" "Do probabilistic reasoning"
   "q" "Quit"
   })

(def QUERIES
  {
   1 "What is the probability that each of the vertices is flooded?"
   2 "What is the probability that each of the edges is blocked?"
   3 "What is the probability that a certain path (set of edges) is free from blockages?"
   })

(defrecord Evidence [evidence location t]
  Object
  (toString [e] (str "Evidence: " (into {} e))))
(defmethod print-method Evidence [gs ^java.io.Writer w] (.write w (str gs)))

(defn get-choice [] (print ">>>") (flush) (read-line))
(defn get-int-choice [] (Integer. (get-choice)))
(defn get-list-choice [] (-> (get-choice) (str/split #" ")))

(defn vertices-or-edges [evidence-index indexed-vertices indexed-edges]
  (let [to-print (if (<= evidence-index 2) indexed-vertices indexed-edges)]
    (doseq [[index item] (into [] to-print)] (println index ">" item))))

(defn vertex-or-edge [evidence-index item-index indexed-vertices indexed-edges]
  (let [m (if (<= evidence-index 2) indexed-vertices indexed-edges)]
    (m (str item-index))))

(defn query-user [indexed-vertices indexed-edges]
  (doseq [[index evidence] EVIDENCE_OPTIONS] (println index ">" evidence))
  (let [evidence-index (get-int-choice)]
    (println (EDGE_OR_VERTEX evidence-index) "?")
    (vertices-or-edges evidence-index indexed-vertices indexed-edges)
    (let [item-index (get-int-choice)
          item (vertex-or-edge evidence-index item-index indexed-vertices indexed-edges)
          t (do (println "Time?") (get-int-choice))]

      (Evidence. (EVIDENCE_OPTIONS evidence-index) item t))))

(defn choose-action []
  (println "\no__O?")
  (doseq [[k description] ACTIONS] (println (<< "[~{k}] ~{description}")))
  (get-choice))

(defn print-evidence [evidences]
  (println)
  (doseq [[i e] (map-indexed #(vector (inc %1) %2) evidences)]
    (println (<< "~{i})~{e}")))
  (println))

(defn vertices-flood-proba
  [bayes t e-map sample-size]
  (let [vertices-t (bn/filter-vertices-t bayes t)
        flood-probs (sampled-proba bayes vertices-t e-map sample-size)]

    (println "Flood probabilities:")
    (doseq [[vertex f-prob] (into [] flood-probs)]
      (println (<< "~{vertex} - ~{f-prob}")))))

(defn edge-blocked-proba
  [bayes t e-map sample-size]
  (let [edges-t (bn/filter-edges-t bayes t)
        block-probs (sampled-proba bayes edges-t e-map sample-size)]

    (println "Block probabilities:")
    (doseq [[vertex f-prob] (into [] block-probs)]
      (println (<< "~{vertex} - ~{f-prob}")))))

(defn get-path-edges []
  (println "Write space separated vertex names in path, eg: 1 2 4")
  (let [path-vs (get-list-choice)
        path-edges (pairwise-collection path-vs)]
    path-edges))

(defn path-free-proba
  [bayes t e-map sample-size]
  (let [path-edges (get-path-edges)
        edges-t (bn/filter-edges-t bayes t)
        block-probs (sampled-proba bayes edges-t e-map sample-size)
        relevant-edges (bn/filter-edges-by-src-dest path-edges edges-t)
        path-block-proba (apply * (map block-probs relevant-edges))
        path-free-p (- 1 path-block-proba)]

    (println "Block probabilities of edges:")
    (doseq [e relevant-edges]
      (println (<< "~{e} - ~{(block-probs e)}")))

    (println "Path free probability:" path-free-p)))


(defn b-node-and-bool
  [b-net e]
  [(bn/find-bayes-node b-net (:location e) (:t e))
   (or (= (:evidence e) :flood) (= (:evidence e) :block))])

(defn evidence-vec-to-map
  [b-net e-vec]
  (let [b-nodes-and-bools (map #(b-node-and-bool b-net %) e-vec)]
    (into {} b-nodes-and-bools)))

(defn print-e-map [e-map]
  (println "\n---------------- Evidence ----------------")
  (doseq [[k v] e-map] (println k "-" v))
  (println "------------------------------------------\n"))

(defn proba-reason
  [bayes evidence sample-size]
  (doseq [[index evidence] QUERIES] (println index ">" evidence))
  (let [reason-index (get-int-choice)
        t (do (println "Time?") (get-int-choice))
        bayes-t (bn/b-net-with-t bayes t)
        evidence-map (evidence-vec-to-map bayes-t evidence)]

    (print-e-map evidence-map)
    (case reason-index

      1 (vertices-flood-proba bayes-t t evidence-map sample-size)
      2 (edge-blocked-proba bayes-t t evidence-map sample-size)
      3 (path-free-proba bayes-t t evidence-map sample-size))))

(defn main-loop
  [graph-desc bayes sample-size]

  (let [nodes (-> graph-desc :props :nodes)
        edges (-> graph-desc :props :edges)]

    (loop [evidences []]


      (let [act (choose-action)]

        (case act
          "q" nil

          "a" (recur (cons (query-user nodes edges) evidences))

          "l" (do (print-evidence evidences) (recur evidences))

          "p" (do (proba-reason bayes evidences sample-size) (recur evidences))

          "r" (recur [])

          )))))