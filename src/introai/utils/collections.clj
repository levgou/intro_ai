(ns introai.utils.collections)

(defn in?
  "true if coll contains elm"
  [coll elm]
  (or (some #(= elm %) coll) false))
