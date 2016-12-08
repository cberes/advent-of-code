(ns advent-of-code.day8
  (:use [clojure.string :only [split-lines trim]]))

(def width 50)
(def height 6)

(def rect-pattern #"^\s*rect\s+(\d+)\s*x\s*(\d+)\s*$")
(def rotate-row-pattern #"^\s*rotate\s+row\s+y\s*=\s*(\d+)\s*by\s*(\d+)\s*$")
(def rotate-column-pattern #"^\s*rotate\s+column\s+x\s*=\s*(\d+)\s*by\s*(\d+)\s*$")

(defn new-cell [x y]
  {:x x :y y :state :off})

(defn initial-state [w h]
  (for [x (range 0 w) y (range 0 h)] (new-cell x y)))

(defn process-rect [cmd state]
  (let [[name a b] cmd]
    (map
      (fn [cell]
        (if (and (< (:x cell) a) (< (:y cell) b))
          (assoc cell :state :on)
          cell))
      state)))

(defn rotate-row [n {x :x}]
  (mod (+ x n) width))

(defn process-rot-row [cmd state]
  (let [[name y x] cmd]
    (map
      (fn [cell]
        (if (= y (:y cell))
          (assoc cell :x (rotate-row x cell))
          cell))
      state)))

(defn rotate-column [n {y :y}]
  (mod (+ y n) height))

(defn process-rot-col [cmd state]
  (let [[name x y] cmd]
    (map
      (fn [cell]
        (if (= x (:x cell))
          (assoc cell :y (rotate-column y cell))
          cell))
      state)))

(defn process-cmd [cmd state]
  (case (first cmd)
    :rect (process-rect cmd state)
    :rot-row (process-rot-row cmd state)
    :rot-col (process-rot-col cmd state)))

(defn process
  ([cmds] (process cmds (initial-state width height)))
  ([cmds state]
    (if (seq cmds)
      (process (rest cmds) (process-cmd (first cmds) state))
      state)))

(defn to-int [s]
  (Integer/parseInt s))

(defn parse-cmd-as-rect [cmd]
  (when-let [matches (re-find rect-pattern cmd)]
    [:rect (to-int (nth matches 1)) (to-int (nth matches 2))]))

(defn parse-cmd-as-rot-row [cmd]
  (when-let [matches (re-find rotate-row-pattern cmd)]
    [:rot-row (to-int (nth matches 1)) (to-int (nth matches 2))]))

(defn parse-cmd-as-rot-col [cmd]
  (when-let [matches (re-find rotate-column-pattern cmd)]
    [:rot-col (to-int (nth matches 1)) (to-int (nth matches 2))]))

(defn parse-cmd [cmd]
  (or (parse-cmd-as-rect cmd)
      (parse-cmd-as-rot-row cmd)
      (parse-cmd-as-rot-col cmd)))

(defn read-lines [file]
  (->> file
    (slurp)
    (split-lines)
    (map trim)))

(defn run [file]
  (->> file
    (read-lines)
    (map parse-cmd)
    (process)
    (filter #(= :on (:state %)))
    (count)))
