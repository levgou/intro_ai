(ns introai.utils.collections)

(defn in?
  "true if coll contains elm"
  [coll elm]
  (or (some #(= elm %) coll) false))

(defn map-to-vec [func collection]
  (apply vector (map func collection)))

(defn map-leading-items
  "will return a map where for each leading pair in the collection
  a {p1 p2} pair will be included in the map, last item will be mapped to nil {p1 nil}"
  [collection]
  (into {} (map-to-vec
             #(apply vector %)
             (partition 2 1 (concat collection '(nil))))))

(defn pairwise-collection [col]
  (vec
    (filter (every-pred first second) (map-leading-items col))))

(defn dissoc-in [mapping keys]
  (update-in mapping (butlast keys) dissoc (last keys)))

(defn maps-agree [constrained constrain]
  (let [ks (keys constrain)]
    (every? #(= (constrained %) (constrain %)) ks)))