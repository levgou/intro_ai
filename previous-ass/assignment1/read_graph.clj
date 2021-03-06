(ns introai.assignment1.read-graph
  (:gen-class)
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [loom.graph :as graph]
            [loom.alg :as alg]
            [introai.assignment1.graph-description :as desc]))

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

(defn parse-int
  [pattern line]
  (Integer. (or (last (re-matches pattern line)) "0")))

(defn name-to-record
  [records]
  (zipmap (map :name records) records))

(defn parse-num-nodes
  [g-list]
  (last (re-matches #".*(\d+)"
                    (first (filter-lines "#N" g-list)))))

(defn parse-node
  [node-line]
  (desc/map->NodeInfo
    {
     :name        (parse-int #"#V(\d+).*" node-line)
     :dead-line   (parse-int #".*D(\d+).*" node-line)
     :num-persons (parse-int #".*P(\d+).*" node-line)
     :has-shelter (str/ends-with? node-line "S")
     }))

(defn parse-nodes
  [g-list]
  (name-to-record (map parse-node
                       (filter-lines "#V" g-list))))

(defn parse-edge
  [edge-line]
  (desc/map->EdgeInfo
    {
     :name   (parse-int #"#E(\d+).*" edge-line)
     :start  (parse-int #"#E\d+ (\d+).*" edge-line)
     :end    (parse-int #"#E\d+ \d+ (\d+).*" edge-line)
     :weight (parse-int #".*W(\d+)" edge-line)
     }))

(defn parse-edges
  [g-list]
  (name-to-record
    (map parse-edge (filter-lines "#E" g-list))))

(defn graph-props-from-list
  [g-list]
  (let [nodes (parse-nodes g-list)]
    (desc/map->GraphProps
      {
       :num-nodes     (parse-num-nodes g-list)
       :shelters      (vec (map :name (filter :has-shelter (vals nodes))))
       :nodes         nodes
       :edges         (parse-edges g-list)
       :time-modifier 0
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

(defn read-graph-from-file
  [file_abs]
  (let [g_props
        (-> file_abs
            read-file-no-blank
            remove-comments
            graph-props-from-list)]
    (desc/->GraphDescription (graph-from-props g_props) g_props)))
