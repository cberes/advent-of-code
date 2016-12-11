(ns advent-of-code.day6
  (:use [advent-of-code.util :only [enumerate read-lines]]))

(defn chars-by-index [messages]
  (->> messages
    (seq)
    (map enumerate)
    (mapcat identity)
    (group-by first)
    (into [])))

(defn char-frequencies [chars]
  (->> chars
    (map second)
    (frequencies)))

(defn get-target-char [chars]
  (->> chars
    (char-frequencies)
    (apply min-key val)
    (first)))

(defn pairs-to-string [pairs]
  (->> pairs
    (sort #(- (first %1) (first %2)))
    (map second)
    (apply str)))

(defn run [file]
  (->> file
    (read-lines)
    (chars-by-index)
    (map (fn [[index chars]] [index (get-target-char chars)]))
    (pairs-to-string)))
