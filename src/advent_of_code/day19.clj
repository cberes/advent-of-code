(ns advent-of-code.day19
  (:import [java.util ArrayList])
  (:use [advent-of-code.util :only [read-lines to-int]]))

(defn elves-count [file]
  (->> file
    (read-lines)
    (first)
    (to-int)))

(defn #^ArrayList initial-state [elf-count]
  (ArrayList. (->> elf-count
                (range)
                (map (fn [index] [index 1])))))

(defn get-giver [state]
  (quot (.size state) 2))

(defn exchange [taker giver]
  [(first taker) (+ (second taker) (second giver))])

(defn play-game [elf-count]
  (loop [state (initial-state elf-count)]
    (if (= (.size state) 1)
      (inc (first (.get state 0)))
      (let [taker-index 0
            taker (.get state taker-index)
            giver-index (get-giver state)
            giver (.get state giver-index)]
        (.remove state (int giver-index))
        (.remove state (int taker-index))
        (.add state (exchange taker giver))
        (recur state)))))

(defn run [file]
  (->> file
    (elves-count)
    (play-game)))

