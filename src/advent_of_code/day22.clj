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

(defn size [nodes]
  [(->> nodes
     (map :coords)
     (map first)
     (apply max))
   (->> nodes
     (map :coords)
     (map second)
     (apply max))])

(defn goal-position [nodes]
  [(first (size nodes)) 0])

(defn find-at [position nodes]
  (->> nodes
    (filter #(= (:coords %) position))
    (first)))

(defn find-by-name [name nodes]
  (->> nodes
    (filter #(= (:name %) name))
    (first)))

(defn move-node [{source-name :node source-avail :avail source-used :used}
                 {dest-name :node dest-avail :avail dest-used :used}
                 node]
  (-> node
    (assoc :used (cond (= (:node node) source-name) 0
                       (= (:node node) dest-name) (+ dest-used source-used)
                       :else (:used node)))
    (assoc :avail (cond (= (:node node) source-name) (+ source-avail source-used)
                        (= (:node node) dest-name) (- dest-avail source-used)
                        :else (:avail node)))))

(defn move [source dest nodes]
  (map (partial move-node source dest) nodes))

(defn solve
  ([nodes] (solve (find-at (goal-position nodes) nodes) nodes))
  ([goal nodes]
   (println goal)
   (let [new-goal (find-by-name (second (viable-pairs goal nodes)) nodes)
         move-goal (partial move goal new-goal)]
     (if (= (:coords goal) [0 0])
       goal
       (solve (first (move-goal [new-goal]))
              (move-goal nodes))))))

(defn run [file]
  (->> file
    (parse-nodes)
    (solve)))
