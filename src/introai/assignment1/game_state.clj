(ns introai.assignment1.game-state
  (:gen-class))

(defrecord GameState
  [
   agent-node,
   time,
   carrying,
   saved,
   terminated,
   remaining-people,
   ])

(defn initial-state [graph-people]
  (GameState. 1, 0, 0, 0, false, graph-people))

(defn progress-time [edge state]
  (assoc state :time (+ (:time state) (:weight edge))))

(defn traverse-edge [edge state]
  (assoc state
    :agent-node (:dest edge)))

(defn rem-people [state]
  (dissoc (:remaining-people state) (:agent-node state)))