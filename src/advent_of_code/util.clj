(ns advent-of-code.util
  (:use [clojure.string :only [split-lines trim]]))

(defn to-int [s]
  (Integer/parseInt s))

(defn to-long [s]
  (Long/parseLong s))

(defn read-lines [file]
  (->> file
    (slurp)
    (split-lines)
    (map trim)))

(defn enumerate [coll]
  (map vector (range) coll))

(defn contains-value? [coll value]
  (some #(= value %) coll))

; credit to http://blog.jayfields.com/2011/08/clojure-apply-function-to-each-value-of.html
(defn update-values [m f & args]
  (reduce (fn [r [k v]] (assoc r k (apply f v args))) {} m))
