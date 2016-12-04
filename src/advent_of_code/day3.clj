(ns advent-of-code.day3
  (:use [clojure.string :only [split split-lines trim]]))

(defn to-int [s]
  (Integer/parseInt s))

(defn triangle? [sides]
  (let [[a b c] (sort sides)]
    (> (+ a b) c)))

(defn read-triples [file]
  (->> file
    (slurp)
    (split-lines)
    (map trim)
    (map #(split % #"\s+"))
    (map #(map to-int %))))

(defn triangles [file]
  (count (filter triangle? (read-triples file))))
