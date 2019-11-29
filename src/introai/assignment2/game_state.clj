(ns introai.assignment2.game-state
  (:gen-class)
  (:require [clojure.core.strint :refer [<<]]
            [nano-id.core :refer [nano-id]]
            [introai.utils.enums :as E]
            [introai.utils.collections :refer [in? dissoc-in]]
            ))

(defrecord GameState
  [
   agent-node,
   carrying,
   saved,
   dead,
   terminated,
   score,
   id,
   ])

(defn initial-state []
  (GameState. 1, 0, 0, {E/DIED-WITH-AGENT 0 E/DIED-IN-CITY 0}, E/NOT-TERMINATED, 0 (nano-id 10)))

(defn traverse-edge [dest state]
  (assoc state
    :agent-node dest))

(defn rem-people [graph-desc state]
  (dissoc-in graph-desc [:remaining-people (:agent-node state)]))

(defn term? [state]
  (in? [E/TERMINATED-UNSAFELY E/TERMINATED-SAFELY] (:terminated state)))