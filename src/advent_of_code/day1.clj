(ns advent-of-code.day1
  (:use [clojure.string :only [split trim]]))

(def next-direction
  {:north {:left :west, :right :east}
   :south {:left :east, :right :west}
   :east {:left :north, :right :south}
   :west {:left :south, :right :north}})

(defn simplify [{north :north, south :south, west :west, east :east}]
  {:north (- north south), :east (- east west)})

(defn set-first-duplicate-location [{locations :locations, duplicate :first-duplicate, :as state}]
  (let [location (simplify state)]
    (if (and (not duplicate) (contains? locations location))
      (assoc state :first-duplicate location)
      state)))

(defn move-once [state heading distance]
  (-> state
      (update heading + distance)
      (set-first-duplicate-location)
      (update :locations conj (simplify state))
      (assoc :heading heading)))

(defn move [{heading :heading :as state} [direction distance]]
  (let [new-heading (direction (heading next-direction))]
    (loop [state state
           distance distance]
      (if (> distance 0)
        (recur (move-once state new-heading 1) (dec distance))
        state))))

(defn start [heading]
  {:heading heading
   :locations #{{:north 0, :east 0}}
   :first-duplicate nil
   :north 0
   :south 0
   :west 0
   :east 0})

(defn walk [moves]
  (loop [moves moves
         state (start :north)]
    (if (seq moves)
      (let [next-state (move state (first moves))]
        (recur (rest moves) next-state))
      state)))

(defn parse-direction [move]
  (let [letter (subs move 0 1)]
    (case letter
      "R" :right
      "L" :left)))

(defn parse-distance [move]
  (Integer/parseInt (subs move 1)))

(defn parse [move]
  [(parse-direction move) (parse-distance move)])

(defn read-moves [file]
  (->>
    (split (slurp file) #",")
    (map trim)
    (map parse)))

(defn count-blocks [{north :north, east :east}]
  (+ (Math/abs north) (Math/abs east)))

(defn blocks [file]
  (let [result (walk (read-moves file))
        destination (simplify result)
        first-duplicate (:first-duplicate result)]
    {:total-distance (count-blocks destination)
     :to-first-duplicate (count-blocks first-duplicate)}))

