(ns advent-of-code.day13
  (:use [advent-of-code.util :only [read-lines to-int]]))

(def initial-position [1 1])

(def goal [31 39])

(defn read-offset [file]
  (->> file
    (read-lines)
    (first)
    (to-int)))

(defn odd-ones? [n]
  (->> (Integer/toString n 2)
    (into [])
    (filter #(= % \1))
    (count)
    (odd?)))

(defn wall-func [file]
  (let [offset (read-offset file)]
    (fn [[x y]]
      (odd-ones? (+ (* x x)
                    (* 3 x)
                    (* 2 x y)
                    (* y y)
                    y
                    offset)))))

(defn to-state [position moves]
  {:position position :moves moves})

(defn get-next-moves [{[x y] :position moves :moves} wall?]
  (->> [[(inc x) y]
        [(dec x) y]
        [x (inc y)]
        [x (dec y)]]
    (filter #(and (>= (first %1) 0) (>= (second %1) 0)))
    (remove wall?)
    (map #(to-state % (inc moves)))))

(defn add-visited [visited moves]
  (into visited (map :position moves)))

(defn solve [file]
  (let [wall? (wall-func file)]
    (loop [position (to-state initial-position 0)
           moves-to-consider (get-next-moves position wall?)
           visited (add-visited #{initial-position} moves-to-consider)]
      (let [next-position (first moves-to-consider)
            next-moves (get-next-moves next-position wall?)
            new-moves (remove #(contains? visited (:position %)) next-moves)]
        (if (= 50 (:moves next-position))
          (count visited)
          (recur
            next-position
            (doall (concat (rest moves-to-consider) new-moves))
            (add-visited visited new-moves)))))))

(defn run [file]
  (solve file))
