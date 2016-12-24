(ns advent-of-code.day21
  (:use [advent-of-code.util :only [read-lines to-int]]))

(defn rotate-left [n v]
  (if (= n 0)
    v
    (rotate-left (dec n) (conj (subvec v 1) (first v)))))

(defn rotate-right [n v]
  (if (= n 0)
    v
    (rotate-right (dec n) (into [(last v)]
                                (subvec v 0 (dec (count v)))))))

(defn command-replace [[x y] v]
  (replace {x y, y x} v))

(defn command-swap [positions v]
  (let [[x y] (sort positions)
        a (nth v x)
        b (nth v y)]
    (-> []
      (into (subvec v 0 x))
      (into [b])
      (into (subvec v (inc x) y))
      (into [a])
      (into (subvec v (inc y))))))

(defn command-rotate [[dir n] v]
  (if (= dir :left)
    (rotate-left n v)
    (rotate-right n v)))

(defn rotate-times [i]
  (+ i (if (> i 3) 2 1)))

(defn undo-rotate-times [char-index coll]
  (let [length (count coll)]
    (->> (range 0 length)
      (map (fn [i] [(rotate-times i) (mod (+ i (rotate-times i)) length)]))
      (filter #(= (second %) char-index))
      (map first))))

(defn command-rotate-extra [[dir letter] v]
  (let [index (.indexOf v letter)]
    (if (= dir :right)
      (rotate-right (rotate-times index) v)
      (rotate-left (last (undo-rotate-times index v)) v))))

(defn command-reverse [[x y] v]
  (-> []
    (into (subvec v 0 x))
    (into (reverse (subvec v x (inc y))))
    (into (subvec v (inc y)))))

(defn command-move [[x y] v]
  (let [x-removed (into (subvec v 0 x)
                        (subvec v (inc x)))]
    (-> (subvec x-removed 0 y)
      (into (subvec v x (inc x)))
      (into (if (= y (count x-removed)) [] (subvec x-removed y))))))

(defn parse-swap [s]
  (let [matches (first (re-seq #"^swap\s+position\s+(\d+)\s+with\s+position\s+(\d+)$" s))]
    (when (seq matches)
      {:f command-swap, :cmd :swap, :args [(to-int (second matches)) (to-int (last matches))]})))

(defn parse-replace [s]
  (let [matches (first (re-seq #"^swap\s+letter\s+(.)\s+with\s+letter\s+(.)$" s))]
    (when (seq matches)
      {:f command-replace, :cmd :replace, :args [(first (second matches)) (first (last matches))]})))

(defn parse-rotate [s]
  (let [matches (first (re-seq #"^rotate\s+(left|right)\s+(\d+)\s+steps?$" s))]
    (when (seq matches)
      {:f command-rotate, :cmd :rotate, :args [(keyword (second matches)) (to-int (last matches))]})))

(defn parse-rotate-extra [s]
  (let [matches (first (re-seq #"^rotate\s+based\s+on\s+position\s+of\s+letter\s+(.)$" s))]
    (when (seq matches)
      {:f command-rotate-extra, :cmd :rotate-extra, :args [:right (first (last matches))]})))

(defn parse-reverse [s]
  (let [matches (first (re-seq #"^reverse\s+positions\s+(\d+)\s+through\s+(\d+)$" s))]
    (when (seq matches)
      {:f command-reverse, :cmd :reverse, :args [(to-int (second matches)) (to-int (last matches))]})))

(defn parse-move [s]
  (let [matches (first (re-seq #"^move\s+position\s+(\d+)\s+to\s+position\s+(\d+)$" s))]
    (when (seq matches)
      {:f command-move, :cmd :move, :args [(to-int (second matches)) (to-int (last matches))]})))

(defn parse-command [s]
  (or (parse-swap s)
      (parse-replace s)
      (parse-rotate s)
      (parse-rotate-extra s)
      (parse-reverse s)
      (parse-move s)))

(defn read-commands [file]
  (->> file
    (read-lines)
    (map parse-command)))

(defn swap-direction [dir]
  (if (= dir :left) :right :left))

(defn undo-command [{cmd-type :cmd args :args :as command}]
  (assoc command :args (cond
                         (= cmd-type :rotate) [(swap-direction (first args)) (second args)]
                         (= cmd-type :rotate-extra) [(swap-direction (first args)) (second args)]
                         (= cmd-type :move) [(second args) (first args)]
                         :else args)))

(defn undo-commands [commands]
  (->> commands
    (reverse)
    (map undo-command)))

(defn process [start commands]
  (if (empty? commands)
    (apply str start)
    (let [{f :f args :args} (first commands)]
      (process (f args start) (rest commands)))))

(defn run [file]
  (->> file
    (read-commands)
    (undo-commands)
    (process (into [] "fbgdceah"))))

