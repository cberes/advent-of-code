(ns advent-of-code.day10
  (:use [advent-of-code.util :only [enumerate read-lines to-int contains-value?]]))

(def input-pattern #"^\s*value\s+(\d+)\s+goes\s+to\s+bot\s+(\d+)\s*$")

(def output-pattern #"^\s*bot\s+(\d+)\s+gives\s+low\s+to\s+(\w+)\s+(\d+)\s+and\s+high\s+to\s+(\w+)\s+(\d+)\s*$")

(defn parse-input [id s]
  (when-let [matches (re-find input-pattern s)]
    {:id id
     :value (to-int (nth matches 1))
     :to {:bot (to-int (nth matches 2))}}))

(defn parse-output [id s]
  (when-let [matches (re-find output-pattern s)]
    {:id id
     :low-to [(if (= "bot" (nth matches 2)) :bot :output) (to-int (nth matches 3))]
     :high-to [(if (= "bot" (nth matches 4)) :bot :output) (to-int (nth matches 5))]
     :from {:bot (to-int (nth matches 1))}}))

(defn parse-command [[id s]]
  (or (parse-input id s)
      (parse-output id s)))

(defn get-thing [type id state]
  (->> state
    (filter #(contains? % type))
    (filter #(= id (type %)))
    (first)))

(defn get-bot [id state]
  (get-thing :bot id state))

(defn ready? [bot]
  (>= (count (:values bot)) 2))

(defn get-next-ready-command [commands state]
  (first (filter #(or (not (contains? %1 :from))
                      (ready? (get-bot (:bot (:from %1)) state))) commands)))

(defn update-bot [value type id bot]
  (if (= id (type bot))
    (update bot :values conj value)
    bot))

(defn add-value-to [value type id state]
  (if-let [bot (get-thing type id state)]
    (map #(update-bot value type id %) state)
    (conj state {type id, :values [value]})))

(defn process-input [command state]
  (let [id (:bot (:to command))
        value (:value command)]
    (add-value-to value :bot id state)))

(defn process-output [command state]
  (let [id (:bot (:from command))
        bot (get-bot id state)
        [low-value high-value] (sort (:values bot))
        [low-type low-id] (:low-to command)
        [high-type high-id] (:high-to command)]
    (->> state
      (map #(if (= id (:bot %1)) (assoc %1 :values []) %1))
      (add-value-to low-value low-type low-id)
      (add-value-to high-value high-type high-id))))

(defn process-cmd [command state]
  (if (contains? command :from)
    (process-output command state)
    (process-input command state)))

(defn target-state [state]
  (->> state
    (filter #(and (contains-value? (:values %1) 17) (contains-value? (:values %1) 61)))
    (first)))

(defn process
  ([commands] (process commands nil))
  ([commands state]
    (when-let [bot (target-state state)]
      (println bot))
    (if (seq commands)
      (let [next-command (get-next-ready-command commands state)
            next-id (:id next-command)
            remaining (remove #(= next-id (:id %)) commands)]
        (process remaining (process-cmd next-command state)))
      state)))

(defn run [file]
  (->> file
    (read-lines)
    (enumerate)
    (map parse-command)
    (process)
    (filter #(seq (:values %)))))
