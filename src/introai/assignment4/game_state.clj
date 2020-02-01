(ns introai.assignment4.game-state
  (:gen-class)
  (:require [clojure.core.strint :refer [<<]]
            [nano-id.core :refer [nano-id]]
            [introai.utils.const :as E]
            [introai.utils.collections :refer [in? dissoc-in]]
            ))

(defrecord GameState
  [
   time
   agent-node
   carrying
   saved
   dead
   terminated
   score
   remaining-people
   id
   ]
  Object
  (toString [gs] (str (into {} gs)))
  )
(defmethod print-method GameState [gs ^java.io.Writer w] (.write w (str gs)))


(defn init-state [start-node remaining-people]
  (GameState. 0 start-node 0 0 {E/DIED-WITH-AGENT 0 E/DIED-IN-CITY 0} E/NOT-TERMINATED 0 remaining-people (nano-id 7)))

(defn progress-time [edge state]
  (assoc state :time (+ (:time state) (:weight edge))))

(defn not-agent-node [edge state]
  (->> edge
       ((juxt :start :end))
       (remove #{(:agent-node state)})
       first))

(defn traverse-edge [edge state]
  (let [dest (not-agent-node edge state)]
    (assoc state :agent-node dest)))

(defn rem-people [state]
  (dissoc (:remaining-people state) (:agent-node state)))

(defn term? [state]
  (in? [E/TERMINATED-UNSAFELY E/TERMINATED-SAFELY] (:terminated state)))

(defn term-safe? [state]
  (in? [E/TERMINATED-SAFELY] (:terminated state)))

(defn died-with-agent-count [state]
  (-> state :dead E/DIED-WITH-AGENT))