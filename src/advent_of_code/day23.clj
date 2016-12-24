(ns advent-of-code.day23
  (:use [clojure.string :only [split]])
  (:use [advent-of-code.util :only [read-lines enumerate to-int]])
  (:use [advent-of-code.day12 :only [parse-command parse-value get-value get-jnz-offset]]))

(def initial-state {"a" 7})

(defn parse-toggle [s]
  (let [tokens (split s #"\s+")]
    (when (= (first tokens) "tgl")
      [:tgl (parse-value (second tokens))])))

(defn parse-command-with-toggle [s]
  (or (parse-command s)
      (parse-toggle s)))

(defn toggle-command [command]
  (if (= (count command) 2)
    (assoc command 0 (if (= (first command) :inc) :dec :inc))
    (assoc command 0 (if (= (first command) :jnz) :cpy :jnz))))

(defn do-toggle [index state [_ arg] commands]
  (let [offset (get-value state arg)
        command-index (+ index offset)]
    (if (or (< command-index 0) (>= command-index (count commands)))
      commands
      (assoc commands command-index (toggle-command (nth commands command-index))))))

(defn process [commands]
  (loop [index 0
         state initial-state
         commands commands]
    (if (>= index (count commands))
      state
      (let [command (nth commands index)]
        (case (first command)
          :inc (recur (inc index) (assoc state (second command) (inc (get-value state (second command)))) commands)
          :dec (recur (inc index) (assoc state (second command) (dec (get-value state (second command)))) commands)
          :jnz (recur (+ index (get-jnz-offset state command)) state commands)
          :cpy (recur (inc index) (assoc state (last command) (get-value state (second command))) commands)
          :tgl (recur (inc index) state (do-toggle index state command commands)))))))

(defn run [file]
  (->> file
    (read-lines)
    (map parse-command-with-toggle)
    (into [])
    (process)))
