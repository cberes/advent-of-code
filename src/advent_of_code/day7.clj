(ns advent-of-code.day7
  (:use [clojure.string :only [split-lines trim]]))

(def repeat-pattern (re-pattern "([a-z])\\1\\1\\1"))

(def abba-pattern (re-pattern "([a-z])([a-z])\\2\\1"))

(def hypernet-pattern (re-pattern "\\[[a-z]*([a-z])([a-z])\\2\\1[a-z]*\\]"))

(defn false-positive? [abba]
  (seq (re-matches repeat-pattern abba)))

(defn get-abba-matches [address]
  (->> address
    (re-seq abba-pattern)
    (map first)
    (remove false-positive?)))

(defn get-hypernet-matches [address]
  (->> address
    (re-seq hypernet-pattern)
    (map first)
    (remove false-positive?)))

(defn supports-tls? [address]
  (and (seq (get-abba-matches address))
       (empty? (get-hypernet-matches address))))

(defn read-lines [file]
  (->> file
    (slurp)
    (split-lines)
    (map trim)))

(defn run [file]
  (->> file
    (read-lines)
    (filter supports-tls?)
    (count)))
