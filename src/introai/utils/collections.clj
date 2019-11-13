(ns introai.utils.collections)

(defn in?
  "true if coll contains elm"
  [coll elm]
  (some #(= elm %) coll))
