(ns advent-of-code.day9
  (:use [clojure.string :only [blank?]])
  (:use [advent-of-code.util :only [read-lines to-int]]))

(def compression-pattern #"(\w*)(\(\d+x\d+\))?")

(def repeat-pattern #"\((\d+)x(\d+)\)")

(defn find-repetition [s]
  (rest (re-find compression-pattern s)))

(defn parse-repeat [s]
  (let [[letters times] (rest (re-find repeat-pattern s))]
    [(to-int letters) (to-int times)]))

(defn do-decompress [letters s]
  [(subs s 0 letters) (subs s letters)])

(defn decompress
  ([s] (decompress 0 s))
  ([result s]
    (let [[text repetition] (find-repetition s)]
      (if (blank? text)
        (if (blank? repetition)
          result
          (let [[letters times] (parse-repeat repetition)
                [insert next] (do-decompress letters (subs s (count repetition)))]
            (decompress (+ result (* times (decompress insert))) next)))
        (decompress (+ result (count text)) (subs s (count text)))))))

(defn run [file]
  (->> file
    (read-lines)
    (first)
    (decompress)))
