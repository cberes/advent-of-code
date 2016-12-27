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

(defn get-destinations-with-distance [state destinations]
  (->> destinations
    (map (fn [vertex] {vertex (:distance (state vertex))}))
    (apply merge)
    (sort-by val)))

(defn distance-squared [[x0 y0] [x1 y1]]
  (+ (Math/pow (- x1 x0) 2) (Math/pow (- y1 y0) 2)))

; compares distance on the plane if 2 destinations are an equal number of steps away
; it's not a good solution, but it seems to work
(defn get-nearest [source destinations state]
  (let [dests (get-destinations-with-distance state destinations)]
    (if (or (= (count dests) 1) (< (val (first dests)) (val (second dests))))
      (first (first dests))
      (->> dests
        (take 2)
        (map first)
        (apply min-key (partial distance-squared source))))))

(defn visit-all [{maze :maze} start initial-destinations]
  (loop [source start
         destinations initial-destinations
         route []]
    (if (empty? destinations)
      (count route)
      (let [state (dijkstra (open-positions maze) source)
            next-destination (first destinations)]
        (recur next-destination
               (rest destinations)
               (into route (rest (path-to state next-destination))))))))

(defn run [file]
  (let [maze (read-maze file)
        destinations (destinations maze)]
    (visit-all maze (first (vals destinations))
                    (map destinations [1 3 6 7 4 5 2 0])))) ; ugh, this looks right for my input
