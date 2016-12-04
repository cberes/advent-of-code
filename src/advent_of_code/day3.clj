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

(defn transpose [[a b c]]
  [[(nth a 0) (nth b 0) (nth c 0)]
   [(nth a 1) (nth b 1) (nth c 1)]
   [(nth a 2) (nth b 2) (nth c 2)]])

(defn fix-triples [triples]
  (->> triples
    (partition 3)
    (map transpose)
    (mapcat identity))) ; this flattens one level of lists

(defn run [file]
  (count (filter triangle? (fix-triples (read-triples file)))))
