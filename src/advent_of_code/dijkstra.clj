(ns advent-of-code.dijkstra
  (:use [clojure.string :only [split-lines trim]]))

(defn initialize [graph source]
  (assoc-in (->> graph
              (map (fn [vertex] {vertex {:distance Integer/MAX_VALUE
                                         :previous nil}}))
              (apply merge)) [source :distance] 0))

(defn neighbor-positions [[x y]]
  [[(inc x) y]
   [(dec x) y]
   [x (inc y)]
   [x (dec y)]])

(defn get-neighbors [unvisited position]
  (->> position
    (neighbor-positions)
    (filter (partial contains? unvisited))))

(defn get-neighbors-map [graph position]
  (->> position
    (neighbor-positions)
    (filter (partial contains? graph))
    (map (fn [position] {position (graph position)}))
    (apply merge)))

(defn get-nearest [unvisited]
  (->> unvisited
    (remove #(nil? (:distance (second %))))
    (sort-by #(:distance (second %)))
    (first)
    (first)))

(defn update-distance [unvisited source node]
  (let [old-distance (:distance (unvisited node))
        new-distance (inc (:distance (unvisited source)))]
    (if (< new-distance old-distance)
      (-> unvisited
        (assoc-in [node :distance] new-distance)
        (assoc-in [node :previous] source))
      unvisited)))

(defn update-distances [unvisited source nodes]
  (if (empty? nodes)
    unvisited
    (update-distances (update-distance unvisited source (first nodes))
                      source
                      (rest nodes))))

(defn dijkstra [graph source]
  (loop [unvisited (initialize graph source)
         visited {}]
    (if (empty? unvisited)
      visited
      (let [position (get-nearest unvisited)
            vertex (unvisited position)
            neighbors (get-neighbors unvisited position)]
        (recur (-> unvisited
                 (update-distances position neighbors)
                 (dissoc position))
               (assoc visited position vertex))))))

(defn path-to [visited node]
  (if node
    (conj (path-to visited (:previous (visited node))) node)
    []))
