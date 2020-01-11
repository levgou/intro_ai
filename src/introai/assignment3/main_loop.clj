(ns introai.assignment3.main-loop
  (:gen-class)
  (:require
    [clojure.core.strint :refer [<<]]
    [introai.assignment3.bayes-network :as bn]
    [introai.assignment3.rand-sample :refer [sampled-proba]]
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
  (println "o__O?")
  (doseq [[k description] ACTIONS] (println (<< "[~{k}] ~{description}")))
  (get-choice))

(defn print-evidence [evidences]
  (println)
  (doseq [[i e] (map-indexed #(vector (inc %1) %2) evidences)]
    (println (<< "~{i})~{e}")))
  (println))

(defn vertices-flood-proba
  [bayes t evidence sample-size]
  (let [vertices-t (bn/filter-vertices-t bayes t)
        flood-probs (sampled-proba bayes vertices-t evidence sample-size)]

    (println "Flood probabilities:")
    (doseq [[vertex f-prob] (into [] flood-probs)]
      (println (<< "~{vertex} - ~{f-prob}")))))

(defn edge-blocked-proba [t])
(defn path-free-proba [t])

(defn proba-reason
  [{persistence :persistence :as bayes} evidence]

  (doseq [[index evidence] QUERIES] (println index ">" evidence))
  (let [reason-index (get-int-choice)
        t (do (println "Time?") (get-int-choice))
        bayes-t (bn/b-net-with-t bayes t)
        evidence-map (evidence-vec-to-map bayes-t evidence)]

    (case reason-index

      1 (vertices-flood-proba bayes-t t evidence persistence)
      2 (edge-blocked-proba t)
      3 (path-free-proba t))))

(defn main-loop
  [graph-desc bayes]

  (let [nodes (-> graph-desc :props :nodes)
        edges (-> graph-desc :props :edges)]

    (loop [evidences []]


      (let [act (choose-action)]

        (case act
          "q" nil

          "a" (recur (cons (query-user nodes edges) evidences))

          "l" (do (print-evidence evidences) (recur evidences))

          "p" (do (proba-reason bayes evidences) (recur evidences))

          "r" (recur [])
          )

        )


      ;(when-not (= query "exit")
      ;
      ;  (query-user)
      ;
      ;  )
      ;
      ;
      )))