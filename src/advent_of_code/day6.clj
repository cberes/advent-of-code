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
    (map #(map vector (range (count %1)) %1)) ; maps chars to pairs with that char's index
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
    (read-messages)
    (chars-by-index)
    (map (fn [[index chars]] [index (get-target-char chars)]))
    (pairs-to-string)))
