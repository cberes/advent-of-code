(ns advent-of-code.day22
  (:use [advent-of-code.util :only [read-lines to-int]]))

(defn parse-coords [s]
  (let [matches (first (re-seq #"^.+-x(\d+)-y(\d+)$" s))]
    (when (seq matches)
      [(to-int (nth matches 1)) (to-int (nth matches 2))])))

(defn parse-node [s]
  (let [matches (first (re-seq #"^(.+?)\s+(\d+)T\s+(\d+)T\s+(\d+)T\s+(\d+)%$" s))]
    (when (seq matches)
      {:node (nth matches 1)
       :coords (parse-coords (nth matches 1))
       :size (to-int (nth matches 2))
       :used (to-int (nth matches 3))
       :avail (to-int (nth matches 4))
       :use-pct (to-int (nth matches 5))})))

(defn parse-nodes [file]
  (->> file
    (read-lines)
    (drop 2)
    (map parse-node)))

(defn viable-pairs
  ([nodes]
   (->> nodes
     (map #(viable-pairs % nodes))
     (filter seq)
     (mapcat identity)))
  ([{name :node used :used} nodes]
   (when (> used 0)
     (->> nodes
       (filter #(not= name (:node %)))
       (filter #(< used (:avail %)))
       (map (fn [other] [name (:node other)]))))))

(defn run [file]
  (->> file
    (parse-nodes)
    (viable-pairs)
    (count)))
