(ns advent-of-code.day4
  (:use [clojure.string :only [split-lines trim]]))

(def letter-offset (int \a))
(def num-letters 26)

(defn shift [c times]
  (if (= c \-)
    \space
    (-> c
      (int)
      (- letter-offset)
      (+ times)
      (mod num-letters)
      (+ letter-offset)
      (char))))

(defn decrypt [[name sector]]
  (let [shift-times (Integer/parseInt sector)
        shifted-chars (map #(shift % shift-times) name)
        shifted (apply str shifted-chars)]
    [shifted sector]))

(defn frequency-comparator [a b]
  (if (= (second a) (second b))
    (compare (first a) (first b))
    (- (second b) (second a))))

(defn remove-non-letters [s]
  (apply str (filter #(Character/isLetter %) s)))

(defn letter-frequencies [s]
  (reduce #(assoc %1 %2 (inc (%1 %2 0))) {} s))

(defn build-checksum [s]
  (->> s
    (remove-non-letters)
    (letter-frequencies)
    (into [])
    (sort frequency-comparator)
    (map first)
    (take 5)
    (apply str)))

(defn real? [[name sector checksum]]
  (= checksum (build-checksum name)))

(defn parse-room-name [room]
  (rest (re-find #"(.+)-(\d+)\[([a-z]+)\]" room)))

(defn read-names [file]
  (->> file
    (slurp)
    (split-lines)
    (map trim)
    (map parse-room-name)))

(defn run [file]
  (->> file
    (read-names)
    (filter real?)
    (map decrypt)))
