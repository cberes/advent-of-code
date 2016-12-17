(ns advent-of-code.day15
  (:use [advent-of-code.util :only [read-lines update-values]]))

(def initial-state
  {1 {:positions 17 :position 5}
   2 {:positions 19 :position 8}
   3 {:positions  7 :position 1}
   4 {:positions 13 :position 7}
   5 {:positions  5 :position 1}
   6 {:positions  3 :position 0}})

(defn update-position [{positions :positions position :position}]
  (mod (inc position) positions)) 

(defn tick [state]
  (update-values state #(assoc %1 :position (update-position %1))))

(defn done? [state capsule]
  (> capsule (key (apply max-key key state))))

(defn can-continue? [state capsule]
  (or (not (contains? state capsule)) (= 0 (:position (get state capsule)))))

(defn solve [initial-state]
  (loop [state initial-state
         capsule 0
         state-when-dropped state
         time-when-dropped 0]
    (cond
      (done? state capsule) time-when-dropped
      (can-continue? state capsule) (recur (tick state) (inc capsule) state-when-dropped time-when-dropped)
      :else (recur (tick state-when-dropped) 0 (tick state-when-dropped) (inc time-when-dropped)))))

(defn run [file]
  (->> initial-state 
    (solve)))
