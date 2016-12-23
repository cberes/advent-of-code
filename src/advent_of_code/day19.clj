(ns advent-of-code.day19
  (:use [advent-of-code.util :only [read-lines to-int]]))

(defn elves-count [file]
  (->> file
    (read-lines)
    (first)
    (to-int)))

(defn initial-state [elf-count]
  (->> elf-count
    (range)
    (map (fn [index] [index 1]))
    (into [])))

(defn play-game [elf-count]
  (loop [state (initial-state elf-count)]
    (if (= (count state) 1)
      (inc (first (first state)))
      (let [[taker giver] (take 2 state)]
        (recur (conj (subvec state 2) [(first taker) (+ (second taker) (second giver))]))))))

(defn run [file]
  (->> file
    (elves-count)
    (play-game)))

