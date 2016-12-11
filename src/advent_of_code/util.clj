(ns advent-of-code.util
  (:use [clojure.string :only [split-lines trim]]))

(defn to-int [s]
  (Integer/parseInt s))

(defn read-lines [file]
  (->> file
    (slurp)
    (split-lines)
    (map trim)))

(defn enumerate [coll]
  (map vector (range) coll))

(defn contains-value? [coll value]
  (some #(= value %) coll))
