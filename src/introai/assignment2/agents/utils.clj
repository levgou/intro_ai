(ns introai.assignment2.agents.utils
  (:require [introai.utils.const :as E]
            [introai.assignment2.agents.game-funcs :refer [gen-next-ops agent-term? agent-heuristic]]
            [introai.utils.log :as log]))

(defn tail-summary
  ([m]
   (tail-summary m []))

  ([m state-vec]
   (loop [cur-m m cur-state-vec state-vec iter 1]
     ;(println [
     ;          (-> cur-m :di-state :state1 :agent-node)
     ;          (-> cur-m :di-state :state2 :agent-node)
     ;          ]
     ;         )
     (if (nil? cur-m)
       cur-state-vec

       (recur
         (-> cur-m :tail)
         (conj cur-state-vec
               (if (nil? (:di-state cur-m))
                 ["end" "!" (apply str (repeat 120 "-"))]
                 [iter (str "; t=" (-> cur-m :di-state :time))
                  [
                   "Alice"
                   (-> cur-m :di-state :state1 :id)
                   (-> cur-m :di-state :state1 :agent-node)
                   (-> cur-m :di-state :state1 :terminated)

                   "    ;  Bob"
                   (-> cur-m :di-state :state2 :id)
                   (-> cur-m :di-state :state2 :agent-node)
                   (-> cur-m :di-state :state2 :terminated)
                   ]]
                 ))
         (inc iter)
         )))))

(defn tail-summary-str [m]
  (let [sum-vec (tail-summary m)]
    (reduce
      (fn [acc [idx t itm]] (str acc "\n       " idx t ") " itm))
      ""
      sum-vec)))

(defn calc-agent-score [state]
  (-
    (:saved state)

    (if (= (:terminated state) E/TERMINATED-UNSAFELY) 2 0)
    (-> state :dead E/DIED-IN-CITY)
    (* 2 (-> state :dead E/DIED-WITH-AGENT))
    ))

(defn both-agents-terminated [di-state agent-order]
  (log/debug "Both agents terminated!")
  (and
    (agent-term? di-state (first agent-order))
    (agent-term? di-state (second agent-order))
    ))

(defn progress-tick [maxmax-props op-agent m-ogds]
  "Progress time in state if it's Bob's turn,
  returns M-OGD"
  (if (not= op-agent (:time-progressor maxmax-props))
    m-ogds
    (map #(update-in % [:di-state :time] inc) m-ogds)))
