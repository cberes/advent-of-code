(ns advent-of-code.day2
  (:use [clojure.string :only [split-lines]]))

(def next-key
  {1 {:up 1, :down 4, :left 1, :right 2}
   2 {:up 2, :down 5, :left 1, :right 3}
   3 {:up 3, :down 6, :left 2, :right 3}
   4 {:up 1, :down 7, :left 4, :right 5}
   5 {:up 2, :down 8, :left 4, :right 6}
   6 {:up 3, :down 9, :left 5, :right 6}
   7 {:up 4, :down 7, :left 7, :right 8}
   8 {:up 5, :down 8, :left 7, :right 9}
   9 {:up 6, :down 9, :left 8, :right 9}})

(defn get-direction [letter]
  (case letter
    "U" :up
    "D" :down
    "L" :left
    "R" :right))

(defn get-key [start moves]
  (if (empty? moves)
    start
    (get-key
      ((get-direction (subs moves 0 1)) (get next-key start))
      (subs moves 1))))

(defn password [file]
  (loop [lines (split-lines (slurp file))
         start 5
         combo []]
    (if (seq lines)
      (let [next-key (get-key start (first lines))]
        (recur (rest lines) next-key (conj combo next-key)))
      combo)))
      
    