(ns advent-of-code.day25
  (:use [clojure.string :only [split]])
  (:use [advent-of-code.util :only [read-lines]])
  (:use [advent-of-code.day12 :only [parse-command parse-value get-value get-jnz-offset]])
  (:use [advent-of-code.day23 :only [parse-toggle do-toggle]]))

(def initial-state {"a" 0})

(defn parse-out [s]
  (let [tokens (split s #"\s+")]
    (when (= (first tokens) "out")
      [:out (parse-value (second tokens))])))

(defn parse-all-commands [s]
  (or (parse-command s)
      (parse-toggle s)
      (parse-out s)))

(defn process [initial-commands]
  (loop [index 0
         initial-a (initial-state "a")
         state initial-state
         commands initial-commands
         output []]
    (if (>= index (count commands))
      state
      (let [command (nth commands index)]
        (case (first command)
          :inc (recur (inc index) initial-a (assoc state (second command) (inc (get-value state (second command)))) commands output)
          :dec (recur (inc index) initial-a (assoc state (second command) (dec (get-value state (second command)))) commands output)
          :jnz (recur (+ index (get-jnz-offset state command)) initial-a state commands output)
          :cpy (recur (inc index) initial-a (assoc state (last command) (get-value state (second command))) commands output)
          :tgl (recur (inc index) initial-a state (do-toggle index state command commands) output)
          :out (let [next-output (get-value state (second command))]
                 (if (and (seq output) (= (last output) next-output))
                   (do
                     (println (inc initial-a))
                     (recur 0 (inc initial-a) (assoc initial-state "a" (inc initial-a)) initial-commands []))
                   (recur (inc index) initial-a state commands (conj output next-output)))))))))

(defn run [file]
  (->> file
    (read-lines)
    (map parse-all-commands)
    (into [])
    (process)))
