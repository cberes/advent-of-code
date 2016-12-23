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
         max-blocked 0]
    (if (empty? ranges)
      (inc max-blocked)
      (let [[low high] (first ranges)]
        (if (< (inc max-blocked) low)
          (inc max-blocked)
          (recur (rest ranges)
                 (max max-blocked high)))))))

(defn run [file]
  (->> file
    (read-ranges)
    (find-min)))

