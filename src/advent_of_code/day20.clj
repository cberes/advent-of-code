(ns advent-of-code.day20
  (:use [clojure.string :only [split]])
  (:use [advent-of-code.util :only [read-lines to-long]]))

(defn read-ranges [file]
  (->> file
    (read-lines)
    (map #(split % #"\-"))
    (map #(map to-long %))
    (map #(apply vector %))
    (sort-by first)))

(defn find-min [ranges]
  (loop [ranges ranges
         allowed-count 0
         max-blocked 0]
    (if (empty? ranges)
      (+ allowed-count (- 4294967296 (inc max-blocked)))
      (let [[low high] (first ranges)]
        (recur (rest ranges)
               (if (< (inc max-blocked) low) (+ allowed-count (- low (inc max-blocked))) allowed-count)
               (max max-blocked high))))))

(defn run [file]
  (->> file
    (read-ranges)
    (find-min)))

