(ns advent-of-code.day6
  (:use [clojure.string :only [split-lines trim]]))

(defn read-messages [file]
  (->> file
    (slurp)
    (split-lines)
    (map trim)))

(defn chars-by-index [messages]
  (->> messages
    (seq)
    (map #(map vector (range (count %1)) %1))
    (mapcat identity)
    (group-by first)))

(defn char-frequencies [chars]
  (->> chars
    (map second)
    (frequencies)))

(defn most-frequent [chars]
  (->> chars
    (char-frequencies)
    (apply max-key val)))

(defn run [file]
  (->> file
    (read-messages)
    (chars-by-index)
    (into [])
    (map (fn [[index chars]] [index (most-frequent chars)]))
    (sort #(- (first %1) (first %2)))
    (map second)
    (map first)
    (apply str)))
