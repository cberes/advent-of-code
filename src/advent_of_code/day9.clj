(ns advent-of-code.day9
  (:use [clojure.string :only [blank? split-lines trim]]))

(def compression-pattern #"(.*?)(\(\d+x\d+\))")

(def repeat-pattern #"\((\d+)x(\d+)\)")

(defn parse-repeat [s]
  (let [[letters times] (rest (re-find repeat-pattern s))]
    [(Integer/parseInt letters) (Integer/parseInt times)]))

(defn do-decompress [[letters times] s]
  (let [to-repeat (subs s 0 letters)
        next (subs s letters)]
    [(apply str (repeat times to-repeat)) next]))

(defn decompress
  ([s] (decompress nil s))
  ([result s]
    (let [match (re-find compression-pattern s)]
      (if (blank? (second match))
        (if (blank? (last match))
          result
          (let [[insert next] (do-decompress (parse-repeat (last match)) (subs s (count (last match))))]
            (decompress (str result insert) next)))
        (decompress (str result (second match)) (subs s (count (second match))))))))

(defn read-lines [file]
  (->> file
    (slurp)
    (split-lines)
    (map trim)))

(defn run [file]
  (->> file
    (read-lines)
    (first)
    (decompress)
    (count)))
