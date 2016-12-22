(ns advent-of-code.day17
  (:use [digest :only [md5]])
  (:use [advent-of-code.util :only [read-lines]]))

(def start [0 0])
(def goal [3 3])
(def size [4 4])

(defn open? [c]
  (or (and (> (int c) (int \a)) (< (int c) (int \g)))
      (and (> (int c) (int \A)) (< (int c) (int \G)))))

(defn parse-state [s]
  (let [[up down left right] (take 4 (md5 (apply str s)))]
    [{:direction :up, :x 0, :y -1, :letter \U, :state (open? up)}
     {:direction :down, :x 0, :y 1, :letter \D, :state (open? down)}
     {:direction :left, :x -1, :y 0, :letter \L, :state (open? left)}
     {:direction :right, :x 1, :y 0, :letter \R, :state (open? right)}]))

(defn valid? [[x y] {dx :x dy :y}]
  (and (>= (+ x dx) 0)
       (< (+ x dx) (first size))
       (>= (+ y dy) 0)
       (< (+ y dy) (second size))))

(defn valid-moves [position code]
  (->> code
    (parse-state)
    (filter #(valid? position %))
    (filter #(:state %))))

(defn get-next-state [{[x y] :position :as start} move]
  (-> start
    (update :code conj (:letter move))
    (update :moves conj (:letter move))
    (assoc :position [(+ x (:x move)) (+ y (:y move))])))

(defn solve-shortest [start initial-code]
  (loop [possible-states [{:code (into [] initial-code), :position start, :moves []}]]
    (let [{position :position, moves :moves, code :code, :as state} (first possible-states)]
      (if (= position goal)
        (apply str moves)
        (let [next-moves (valid-moves position code)
              next-states (map (partial get-next-state state) next-moves)]
          (recur (doall (concat (rest possible-states) next-states))))))))

(defn solve-longest [start initial-code]
  (loop [possible-states [{:code (into [] initial-code), :position start, :moves []}]
         longest-solution -1]
    (let [{position :position, moves :moves, code :code, :as state} (first possible-states)]
      (cond
        (nil? state) longest-solution
        (= position goal) (recur (rest possible-states) (count moves))
        :else (let [next-moves (valid-moves position code)
                    next-states (map (partial get-next-state state) next-moves)]
                (recur (doall (concat (rest possible-states) next-states)) longest-solution))))))

(defn read-input [file]
  (->> file
    (read-lines)
    (first)))

(defn run [file]
  (->> file
    (read-input)
    (solve-longest start)))
