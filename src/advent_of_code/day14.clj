(ns advent-of-code.day14
  (:use [digest :only [md5]])
  (:use [advent-of-code.util :only [read-lines enumerate]]))

(def stretch-iterations 2016)

(def target-key-count 64)

(def repeat-3-pattern #"(\w)\1\1")
(def repeat-5-pattern #"(\w)\1\1\1\1")

(defn find-repeats [s]
  [(take 1 (map second (re-seq repeat-3-pattern s)))
   (take 1 (map second (re-seq repeat-5-pattern s)))])

(defn complete? [item]
  (not (nil? (:confirmed item))))

(defn update-key [index repeats item]
  (if (and (not (complete? item))
           (some #(contains? (:chars item) %) repeats))
    (assoc item :confirmed index)
    item))

(defn confirm-keys [index repeats state]
  (if (seq repeats)
    (map (partial update-key index repeats) state)
    state))

(defn new-keys [index hash repeats state]
  (if (seq repeats)
    (conj state {:index index, :key hash, :chars (into #{} repeats), :confirmed nil})
    state))

(defn extract-keys [[index hash] state]
  (let [[repeat-3s repeat-5s] (find-repeats hash)]
    (->> state
      (remove #(and (not (complete? %1)) (> index (+ 1000 (:index %1)))))
      (confirm-keys index repeat-5s)
      (new-keys index hash repeat-3s))))

(defn get-complete-keys [state]
  (take target-key-count (sort-by :index (filter complete? state))))

(defn get-keys [initial-hashes]
  (loop [hashes initial-hashes state []]
    (let [complete-keys (get-complete-keys state)
          last-index-found (:index (last (sort-by :index complete-keys)))
          current-index (first (first hashes))]
      (if (and (= (count complete-keys) target-key-count)
               (> current-index (+ 1000 last-index-found)))
        complete-keys
        (recur (rest hashes) (extract-keys (first hashes) state))))))

(defn stretch-hash [text]
  (loop [hash text n 0]
    (if (> n stretch-iterations)
      hash
      (recur (md5 hash) (inc n)))))

(defn hashes [prefix]
  (map (fn [i] [i (stretch-hash (str prefix i))]) (range)))

(defn read-salt [file]
  (first (read-lines file)))

(defn run [file]
  (->> file
    (read-salt)
    (hashes)
    (get-keys)
    (enumerate)))
