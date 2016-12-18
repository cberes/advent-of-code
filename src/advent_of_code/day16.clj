(ns advent-of-code.day16
  (:use [advent-of-code.util :only [read-lines update-values]]))

(def target-length 272)

(defn checksum
  ([s] (checksum s ""))
  ([s cs]
   (cond
     (and (empty? s) (odd? (count cs))) cs
     (and (empty? s) (even? (count cs))) (checksum cs)
     :else (let [[a b] (take 2 s)]
             (checksum (subs s 2) (str cs (if (= a b) \1 \0)))))))

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
    (recursive-copy target-length)))
