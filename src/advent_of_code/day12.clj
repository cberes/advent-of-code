(ns advent-of-code.day12
  (:use [clojure.string :only [split]])
  (:use [advent-of-code.util :only [read-lines enumerate to-int]]))

(def initial-state {"c" 1})

(defn parse-value [s]
  (try
    (to-int s)
    (catch NumberFormatException _ s)))

(defn parse-command [s]
  (let [tokens (split s #"\s+")]
    (case (first tokens)
      "inc" [:inc (second tokens)]
      "dec" [:dec (second tokens)]
      "jnz" [:jnz (parse-value (second tokens)) (parse-value (last tokens))]
      "cpy" [:cpy (parse-value (second tokens)) (last tokens)]
      nil)))

(defn get-register [state register]
  (or (state register) 0))

(defn get-value [state source]
  (if (integer? source)
    source
    (get-register state source)))

(defn get-jnz-offset [state [op value offset]]
  (if (not= 0 (get-value state value))
    (get-value state offset)
    1))

(defn process [commands]
  (loop [index 0
         state initial-state]
    (if (< index (count commands))
      (let [command (nth commands index)]
        (case (first command)
          :inc (recur (inc index) (assoc state (second command) (inc (get-value state (second command)))))
          :dec (recur (inc index) (assoc state (second command) (dec (get-value state (second command)))))
          :jnz (recur (+ index (get-jnz-offset state command)) state)
          :cpy (recur (inc index) (assoc state (last command) (get-value state (second command))))))
      state)))

(defn run [file]
  (->> file
    (read-lines)
    (map parse-command)
    (process)))
