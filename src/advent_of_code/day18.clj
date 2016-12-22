(ns advent-of-code.day18
  (:use [advent-of-code.util :only [read-lines]]))

(def target-rows 400000)

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
    (map (partial new-cell previous))))

(defn safe-count-row [row]
  (->> row
    (filter safe?)
    (count)))

(defn count-safe-tiles [n row]
  (loop [n n
         last-row (into [] row)
         total 0]
    (if (= 0 n)
      total
      (let [current-row-count (safe-count-row last-row)
            new-row (build-row last-row)]
        (recur (dec n) new-row (+ current-row-count total))))))

(defn read-first-row [file]
  (->> file
    (read-lines)
    (first)))

(defn run [file]
  (->> file
    (read-first-row)
    (count-safe-tiles target-rows)))

