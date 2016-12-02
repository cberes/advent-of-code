(ns advent-of-code.day1
  (:use [clojure.string :only [split trim]]))

(def next-direction
  {:north {:left :west, :right :east}
   :south {:left :east, :right :west}
   :east {:left :north, :right :south}
   :west {:left :south, :right :north}})

(defn move [{heading :heading :as state} [direction distance]]
  (let [new-heading (direction (heading next-direction))]
    (-> state
        (update new-heading + distance)
        (assoc :heading new-heading))))

(defn parse-direction [move]
  (let [letter (subs move 0 1)]
    (case letter
      "R" :right
      "L" :left)))

(defn parse-distance [move]
  (Integer/parseInt (subs move 1)))

(defn parse [move]
  [(parse-direction move) (parse-distance move)])

(defn start [heading]
  {:heading heading, :north 0, :south 0, :west 0, :east 0})

(defn walk [moves]
  (loop [moves moves
         state (start :north)]
    (if (seq moves)
      (recur (rest moves) (move state (first moves)))
      state)))

(defn read-moves [file]
  (->>
    (split (slurp file) #",")
    (map trim)
    (map parse)))

(defn count-blocks [{north :north, south :south, west :west, east :east}]
  (+ (Math/abs (- north south)) (Math/abs (- east west))))

(defn blocks [file]
  (count-blocks (walk (read-moves file))))

