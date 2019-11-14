(ns introai.utils.log)

(defn info [& things]
      (apply println (cons "INFO:  " things)))

(defn debug [& things]
  ;(apply println (cons "DEBG:  " things))
  nil
  )

(defn spy [thing] (do (println "SPYY: " thing) thing))
