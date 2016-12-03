(ns advent-of-code.day2
  (:use [clojure.string :only [split-lines]]))

(def next-key
  {\1 {:up \1, :down \3, :left \1, :right \1}
   \2 {:up \2, :down \6, :left \2, :right \3}
   \3 {:up \1, :down \7, :left \2, :right \4}
   \4 {:up \4, :down \8, :left \3, :right \4}
   \5 {:up \5, :down \5, :left \5, :right \6}
   \6 {:up \2, :down \A, :left \5, :right \7}
   \7 {:up \3, :down \B, :left \6, :right \8}
   \8 {:up \4, :down \C, :left \7, :right \9}
   \9 {:up \9, :down \9, :left \8, :right \9}
   \A {:up \6, :down \A, :left \A, :right \B}
   \B {:up \7, :down \D, :left \A, :right \C}
   \C {:up \8, :down \C, :left \B, :right \C}
   \D {:up \B, :down \D, :left \D, :right \D}})

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
         start \5
         combo []]
    (if (seq lines)
      (let [next-key (get-key start (first lines))]
        (recur (rest lines) next-key (conj combo next-key)))
      (apply str combo))))
