(ns advent-of-code.core
  (:use [advent-of-code.day1 :only [blocks]])
  (:use [advent-of-code.day2 :only [password]])
  (:gen-class))

(defn get-task [day]
  (case day
    1 blocks
    2 password))

(defn run-task [[day file]]
  ((get-task (Integer/parseInt day)) file))

(defn -main [& args]
  (println (run-task args)))
