(ns advent-of-code.core
  (:use [advent-of-code.day1 :only [blocks]])
  (:gen-class))

(defn -main [& args]
  (println (blocks (first args))))
