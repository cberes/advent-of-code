(ns advent-of-code.day18
  (:use [advent-of-code.util :only [read-lines]]))

(def target-rows 40)

(defn safe? [tile]
  (= tile \.))

(defn trap? [tile]
  (= tile \^))

(defn new-trap? [[left center right]]
  (or (and (trap? left) (trap? center) (safe? right))
      (and (safe? left) (trap? center) (trap? right))
      (and (trap? left) (safe? center) (safe? right))
      (and (safe? left) (safe? center) (trap? right))))

(defn get-lcr [previous index]
  (cond
    (= index 0) [\.
                 (nth previous 0)
                 (nth previous 1)]
    (= index (dec (count previous))) [(nth previous (- (count previous) 2))
                                      (nth previous (- (count previous) 1))
                                      \.]
    :else [(nth previous (dec index))
           (nth previous index)
           (nth previous (inc index))]))

(defn new-cell [previous index]
  (if (new-trap? (get-lcr previous index)) \^ \.))

(defn build-row [previous]
  (->> previous
    (count)
    (range 0)
    (map (partial new-cell previous))
    (apply str)))

(defn build-rows [n rows]
  (if (= (count rows) n)
    rows
    (let [new-row (build-row (last rows))]
      (build-rows n (conj rows new-row)))))

(defn safe-count-row [row]
  (->> row
    (into [])
    (filter safe?)
    (count)))

(defn safe-count
  ([rows] (safe-count rows 0))
  ([rows total]
   (if (empty? rows)
     total
     (safe-count (rest rows) (+ (safe-count-row (first rows)) total)))))

(defn read-first-row [file]
  (->> file
    (read-lines)
    (first)))

(defn run [file]
  (->> file
    (read-first-row)
    (vector)
    (build-rows target-rows)
    (safe-count)))

