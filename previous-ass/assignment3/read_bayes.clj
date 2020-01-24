(ns introai.assignment3.read-bayes
  (:gen-class)
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [loom.graph :as graph]
            [introai.assignment3.graph-description :as desc]))

(defn read-file-no-blank
  [file_abs]
  (with-open [rdr (io/reader file_abs)]
    (apply list (remove str/blank? (line-seq rdr)))))

(defn remove-comments
  [file-lines]
  (map
    #(str/trim (first (str/split % #";")))
    file-lines))

(defn filter-lines
  [start collection]
  (filter #(str/starts-with? % start) collection))

(defn parse-str-num
  [pattern line]
  (or (last (re-matches pattern line)) "0"))

(defn parse-int
  [pattern line]
  (Integer. (parse-str-num pattern line)))

(defn parse-double
  [pattern line]
  (Double. (parse-str-num pattern line)))

(defn name-to-record
  [records]
  (zipmap (map :name records) records))

(defn parse-num-nodes
  [g-list]
  (last (re-matches #".*(\d+)"
                    (first (filter-lines "#N" g-list)))))

(defn parse-node-name
  [node-line]
  (parse-str-num #"#V(\d+).*" node-line))

(defn parse-node-flood-proba
  [node-line]
  (parse-double #"#V\d.*F (.*)$" node-line))

(defn parse-node
  [node-line flood-probas-map]
  (let [node-name (parse-node-name node-line)]
    (desc/map->NodeInfo
      {
       :name node-name
       :dead-line   (parse-int #".*D(\d+).*" node-line)
       :num-persons (parse-int #".*P(\d+).*" node-line)
       :has-shelter (str/ends-with? node-line "S")
       :flood-prob (flood-probas-map node-name 0.0)
       })))

(defn parse-nodes-flood-proba
  [g-list]
  (->> g-list
       (filter-lines "#V")
       (filter #(str/includes? % " F "))
       (map (juxt parse-node-name parse-node-flood-proba))
       (into {})))

(defn parse-nodes-props
  "will return a map {node-name NodeInfo}"
  [g-list flood-probas]
  (->> g-list
       (filter-lines "#V")
       ;(filter #(not (str/includes? % " F ")))
       (map #(parse-node % flood-probas))
       (name-to-record)))

(defn parse-nodes
  [g-list]
  (let [flood-probas (parse-nodes-flood-proba g-list)
        node-props (parse-nodes-props g-list flood-probas)]
    node-props))

(defn parse-edge
  [edge-line]
  (desc/map->EdgeInfo
    {
     :name   (parse-str-num #"#E(\d+).*" edge-line)
     :start  (parse-str-num #"#E\d+ (\d+).*" edge-line)
     :end    (parse-str-num #"#E\d+ \d+ (\d+).*" edge-line)
     :weight (parse-int #".*W(\d+)" edge-line)
     }))

(defn parse-edges
  [g-list]
  (name-to-record
    (map parse-edge (filter-lines "#E" g-list))))

(defn parse-persist
  [g-list]
  (->> g-list
       (filter-lines "#P")
       first
       (parse-double #"#Ppersistence (.*)$")))

(defn graph-props-from-list
  [g-list]
  (let [node-props (parse-nodes g-list)]
    (desc/map->GraphProps
      {
       :num-nodes (parse-num-nodes g-list)
       :nodes     node-props
       :edges     (parse-edges g-list)
       :persist   (parse-persist g-list)
       })))

(defn list-of-edge-vectors
  [g-props]
  (map
    #(vec (vals (select-keys % [:start :end :weight])))
    (vals (:edges g-props))))

(defn graph-from-props
  [g-props]
  (let [edge-list (list-of-edge-vectors g-props)]
    (apply graph/weighted-graph edge-list)))

(defn props-from-file
  [file-abs]
  (-> file-abs
      read-file-no-blank
      remove-comments
      graph-props-from-list))

(defn name->init-proba [g-props]
  (->>
    g-props
    :nodes
    vals
    (map #(vector (:name %) (:flood-prob %)))
    (into {})))

(defn read-graph-from-file
  [file-abs]
  (let [g-props (props-from-file file-abs)]
    (desc/->GraphDescription
      (graph-from-props g-props)
      g-props
      (desc/people-map (:nodes g-props))
      )))
