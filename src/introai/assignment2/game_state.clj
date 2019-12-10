(ns introai.assignment2.game-state
  (:gen-class)
  (:require [clojure.core.strint :refer [<<]]
            [nano-id.core :refer [nano-id]]
            [introai.utils.const :as E]
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
   ]
  Object
  (toString [gs] (str (into {} gs)))
  )

(defmethod print-method GameState [gs ^java.io.Writer w] (.write w (str gs)))

(defrecord TwoAgentState
  [
   state1
   state2
   time
   id
   ]
  Object
  (toString [tas] (str (into {} tas)))
  )

(defmethod print-method TwoAgentState [tas ^java.io.Writer w] (.write w (str tas)))

(defn state-of [di-state agent]
  (if (= (:name agent) "Alice")
    (:state1 di-state)
    (:state2 di-state)))

(defn state-piece-of [di-state agent key-name]
  (key-name (state-of di-state agent)))

(defn assoc-in-agent [di-state agent key-name value]
  (let [agent-key (if (= (:name agent) "Alice") :state1 :state2)]
    (assoc-in di-state [agent-key key-name] value)))

(defn initial-di-state []
  (let [new-state
        (GameState. "1", 0, 0, {E/DIED-WITH-AGENT 0 E/DIED-IN-CITY 0}, E/NOT-TERMINATED, 0, (nano-id 10))]
    (TwoAgentState. new-state new-state 0 (nano-id 10))))

(defn progress-time [di-state time-amount]
  (update di-state :time + time-amount))

(defn traverse-edge [di-state agent dest]
  (assoc-in-agent di-state agent :agent-node dest))

(defn rem-people [graph-desc state]
  (dissoc-in graph-desc [:remaining-people (:agent-node state)]))

(defn term? [state]
  (in? [E/TERMINATED-UNSAFELY E/TERMINATED-SAFELY] (:terminated state)))