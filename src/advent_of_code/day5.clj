(ns advent-of-code.day5
  (:use [clojure.string :only [starts-with? trim]])
  (:use [digest :only [md5]]))

(defn hashes [prefix]
  (map #(md5 (str prefix %)) (range (Integer/MAX_VALUE))))
  
(defn run [file]
  (->> file
    (slurp)
    (trim)
    (hashes)
    (filter #(starts-with? % "00000"))
    (take 8)
    (map #(nth % 5))
    (apply str)))
