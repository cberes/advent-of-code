(ns advent-of-code.day24
  (:use [advent-of-code.dijkstra :only [dijkstra path-to]])
  (:use [advent-of-code.util :only [read-lines enumerate to-int]]))

(defn read-maze-cell [x y cell]
  {[x y] {:open (not= cell \#)
          :destination (and (not= cell \#)
                            (not= cell \.)
                            (to-int (str cell)))}})

(defn read-maze-row [[y row]]
  (->> row
    (into [])
    (enumerate)
    (map #(read-maze-cell (first %1) y (second %1)))
    (apply merge)))

(defn read-maze [file]
  (let [lines (read-lines file)]
    {:size [(count (first lines)) (count lines)]
     :maze (->> file
             (read-lines)
             (enumerate)
             (map read-maze-row)
             (apply merge))}))

(defn destinations [{maze :maze}]
  (->> maze
    (filter #(:destination (second %)))
    (sort-by #(:destination (second %)))
    (map (fn [cell] {(:destination (second cell)) (first cell)}))
    (apply merge)))

(defn open-positions [maze]
  (->> maze
    (filter #(:open (second %)))
    (map first)))

(defn get-nearest [destinations state]
  (->> destinations
    (map (fn [vertex] {vertex (:distance (state vertex))}))
    (apply merge)
    (apply min-key val)
    (first)))

(defn visit-all [{maze :maze} start initial-destinations]
  (loop [source start
         destinations initial-destinations
         route []]
    (if (empty? destinations)
      (count route)
      (let [state (dijkstra (open-positions maze) source)
            next-destination (get-nearest destinations state)]
        (recur next-destination
               (remove #(= next-destination %) destinations)
               (into route (rest (path-to state next-destination))))))))

(defn run [file]
  (let [maze (read-maze file)
        destinations (vals (destinations maze))]
    (visit-all maze (first destinations) (rest destinations))))
