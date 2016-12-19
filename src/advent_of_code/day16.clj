(ns advent-of-code.day16
  (:use [advent-of-code.util :only [read-lines update-values]]))

(def target-length 35651584)

(defn checksum
  ([s] (checksum (into [] s) []))
  ([s cs]
   (loop [s s cs cs]
     (cond
       (and (empty? s) (odd? (count cs))) (apply str cs)
       (and (empty? s) (even? (count cs))) (recur cs [])
       :else (let [[a b] (take 2 s)]
               (recur (drop 2 s) (conj cs (if (= a b) \1 \0))))))))

(defn copy [a]
  (str a "0" (->> a
               (reverse)
               (replace {\0 \1 \1 \0})
               (apply str))))

(defn recursive-copy [target data]
  (if (>= (count data) target)
    (let [xdata (subs data 0 target)]
      {:data xdata :checksum (checksum xdata)})
    (recursive-copy target (copy data))))

(defn read-input [file]
  (->> file
    (read-lines)
    (first)))

(defn run [file]
  (->> file
    (read-input)
    (recursive-copy target-length)
    (:checksum)))
