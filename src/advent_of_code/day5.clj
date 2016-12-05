(ns advent-of-code.day5
  (:use [clojure.string :only [includes? starts-with? trim]])
  (:use [digest :only [md5]]))

(def password-length 8)
(def zeroes 5)
(def n-zeroes (apply str (take zeroes (repeat "0"))))
(def position-index zeroes)

(defn filter-unique-positions
  ([pairs]
    (filter-unique-positions nil pairs))
  ([to-keep pairs]
    (let [positions (set (map first to-keep))]
      (if (and (seq pairs) (< (count positions) password-length))
        (if (contains? positions (first (first pairs)))
          (filter-unique-positions to-keep (rest pairs))
          (filter-unique-positions (conj to-keep (first pairs)) (rest pairs)))
        to-keep))))

(defn to-pair [s]
  (let [char-index (inc position-index)
        position-char (subs s position-index char-index)
        password-char (nth s char-index)]
    [(Integer/parseInt position-char) password-char]))

(defn hashes [prefix]
  (map #(md5 (str prefix %)) (range (Integer/MAX_VALUE))))

(defn possible-position-pairs [file]
  (->> file
    (slurp)
    (trim)
    (hashes)
    (filter #(starts-with? % n-zeroes))
    (filter #(Character/isDigit (nth % position-index)))
    (map to-pair)
    (filter #(< (first %) password-length))))

(defn run [file]
  (->> file
    (possible-position-pairs)
    (filter-unique-positions)
    (sort #(- (first %1) (first %2)))
    (map second)
    (apply str)))
