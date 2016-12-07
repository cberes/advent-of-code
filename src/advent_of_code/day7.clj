(ns advent-of-code.day7
  (:use [clojure.string :only [split-lines trim]]))

(def hypernet-pattern (re-pattern "\\[([a-z]+?)\\]"))

(def supernet-pattern (re-pattern "(?:^|\\])([a-z]+?)(?:\\[|$)"))

(defn abba? [s]
  (let [[a b c d] (into [] s)]
    (and (= a d) (= b c) (not= a b))))

(defn aba? [s]
  (let [[a b c] (into [] s)]
    (and (= a c) (not= a b))))

(defn split [n s]
  (map #(subs s %1 (+ %1 n)) (range 0 (- (count s) (dec n)))))

(defn get-all-abba [net]
  (filter abba? (split 4 net)))

(defn get-all-aba [net]
  (filter aba? (split 3 net)))

(defn invert [net]
  (str (second net) (first net) (second net)))

(defn separate [address]
  [(map second (re-seq supernet-pattern address))
   (map second (re-seq hypernet-pattern address))])

(defn supports-tls? [address]
  (let [[supernets hypernets] (separate address)
        all-supernet-abba (set (flatten (map get-all-abba supernets)))
        all-hypernet-abba (set (flatten (map get-all-abba hypernets)))]
    (and (seq all-supernet-abba) (empty? all-hypernet-abba))))

(defn supports-ssl? [address]
  (let [[supernets hypernets] (separate address)
        all-supernet-aba (set (flatten (map get-all-aba supernets)))
        all-hypernet-aba (set (flatten (map get-all-aba hypernets)))]
    (some #(contains? all-hypernet-aba %) (map invert all-supernet-aba))))

(defn read-lines [file]
  (->> file
    (slurp)
    (split-lines)
    (map trim)))

(defn run [file]
  (let [addresses (read-lines file)]
    {:tls (count (filter supports-tls? addresses))
     :ssl (count (filter supports-ssl? addresses))}))
