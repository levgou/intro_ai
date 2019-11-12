(ns introai.assignment1.read-graph
  (:gen-class)
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [ubergraph.core :as uber]))


(defrecord NodeInfo [name dead-line num-persons has-shelter])

(defrecord EdgeInfo [name start end weight])

(defrecord GraphDescription [structure props])


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
  (Integer. (last (or (re-matches pattern line) '("0")))))


(defn name-to-record
  [records]
  (into (hash-map) (map vector (map :name records) records)))

(defn parse-num-nodes
  [g-list]
  (last (re-matches #".*(\d+)"
                    (first (filter-lines "#N" g-list)))))

(defn parse-node
  [node-line]
  (map->NodeInfo
    {
     :name        (parse-int #"#V(\d+).*" node-line)        ;(last (re-matches #"#V(\d+).*" node-line))
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
  (map->EdgeInfo
    {
     :name   (parse-int #"#E(\d+).*" edge-line)
     :start  (parse-int #"#E\d+ (\d+).*" edge-line)
     :end    (parse-int #"#E\d+ \d+ (\d+).*" edge-line)
     :weight (parse-int #".*W(\d+)" edge-line)
     }))

(defn parse-edges
  [g-list]
  (name-to-record (map parse-edge
                       (filter-lines "#E" g-list))))

(defn graph-props-from-list
  [g-list]
  {
   :num-nodes (parse-num-nodes g-list)
   :nodes     (parse-nodes g-list)
   :edges     (parse-edges g-list)
   })

(defn list-of-edge-vectors
  [g-props]
  (map
    #(vec (vals (select-keys % [:start :end :weight])))
    (vals (g-props :edges))))

(defn graph-from-props
  [g-props]
  (apply uber/graph (list-of-edge-vectors g-props)))

(defn read-graph-from-file
  [file_abs]
  (let [g_props
        (-> file_abs
            read-file-no-blank
            remove-comments
            graph-props-from-list)]

    map->GraphDescription {
                           :props     g_props
                           :structure (graph-from-props g_props)
                           }))
