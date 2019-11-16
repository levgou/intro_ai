(ns introai.assignment1.game-state
  (:gen-class)
  (:require [clojure.core.strint :refer [<<]]
            [nano-id.core :refer [nano-id]]
            [introai.utils.enums :as E]
            [introai.utils.collections :refer [in?]]
            ))

(defrecord GameState
  [
   agent-node,
   time,
   carrying,
   saved,
   dead,
   terminated,
   remaining-people,
   id,
   ])

(defn initial-state [graph-people]
  (GameState. 1, 0, 0, 0, {E/DIED-WITH-AGENT 0 E/DIED-IN-CITY 0}, E/NOT-TERMINATED, graph-people (nano-id 10)))

(defn progress-time [edge state]
  (assoc state :time (+ (:time state) (:weight edge))))

(defn traverse-edge [edge state]
  (assoc state
    :agent-node (:dest edge)))

(defn rem-people [state]
  (dissoc (:remaining-people state) (:agent-node state)))

(defn term? [state]
  (in? [E/TERMINATED-UNSAFELY E/TERMINATED-SAFELY] (:terminated state)))