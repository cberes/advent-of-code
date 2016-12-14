(ns advent-of-code.day11
  (:use [clojure.data.priority-map])
  (:use [advent-of-code.util :only [read-lines enumerate]]))

(def elevator-start 0)

(def priority-multiplier 5)

(def generator-pattern #"(\w+)\s+generator")

(def microchip-pattern #"(\w+)\W+compatible\s+microchip")

(defn parse-generators [s]
  (map second (re-seq generator-pattern s)))

(defn parse-microchips [s]
  (map second (re-seq microchip-pattern s)))

(defn parse-line [[floor line]]
  (let [generators (parse-generators line)
        microchips (parse-microchips line)]
    (when (or (seq generators) (seq microchips))
      (-> []
        (into (map (fn [g] {:element g, :type :generator, :floor floor}) generators))
        (into (map (fn [m] {:element m, :type :microchip, :floor floor}) microchips))))))

(defn parse-lines [lines]
  (->> lines
    (enumerate)
    (map parse-line)
    (remove nil?)
    (mapcat identity)
    (apply vector)))

(defn read-state [file]
  (let [lines (read-lines file)]
    {:moves 0
     :elevator elevator-start
     :floors (count lines)
     :items (parse-lines lines)}))

(defn generator-elements [items]
  (->> items
    (filter #(= :generator (:type %)))
    (map :element)
    (into #{})))

(defn every-microchip-has-generator? [items]
  (let [generators (generator-elements items)]
    (->> items
      (filter #(= :microchip (:type %)))
      (map :element)
      (every? #(contains? generators %)))))

(defn items-on-floor [state floor]
  (filter #(= floor (:floor %)) (:items state)))

(defn floor-valid? [state floor]
  (let [items (items-on-floor state floor)]
    (or (empty? items)
        (every? #(= :generator (:type %)) items)
        (every? #(= :microchip (:type %)) items)
        (every-microchip-has-generator? items))))

(defn item-floors [state]
  (conj (map :floor (:items state)) (:elevator state)))

(defn valid? [state]
  (let [top-floor (:floors state)]
    (and (every? #(and (>= %1 0) (< %1 top-floor)) (item-floors state))
         (every? #(floor-valid? state %) (range top-floor)))))

(defn indices-to-change [state]
  (->> state
    (:items)
    (count)
    (range)
    (filter #(= (:elevator state) (:floor (nth (:items state) %))))))

(defn update-floor [op item]
  (update item :floor op))

; TODO could refactor next 2 methods to share code
(defn single-floor-change [state op]
  (let [indices (indices-to-change state)]
    (map (fn [index]
           (-> state
             (update :moves inc)
             (update :elevator op)
             (update-in [:items index] (partial update-floor op))))
         indices)))

(defn double-floor-change [state op]
  (let [single-indices (indices-to-change state)
        indices (for [a single-indices b single-indices :when (< a b)] [a b])]
    (map (fn [[a b]]
           (-> state
             (update :moves inc)
             (update :elevator op)
             (update-in [:items a] (partial update-floor op))
             (update-in [:items b] (partial update-floor op))))
         indices)))

(defn next-states [state]
  (filter valid?
          (-> []
            (into (single-floor-change state inc))
            (into (double-floor-change state inc))
            (into (single-floor-change state dec))
            (into (double-floor-change state dec)))))

(defn done? [state]
  (every? #(= (:floor %) (dec (:floors state))) (:items state)))

; idea for priority calculation came from
; https://www.reddit.com/r/adventofcode/comments/5hoia9/2016_day_11_solutions/db1zbu0/
(defn priority [state]
  (let [top-floor (dec (:floors state))]
    (- (:moves state)
       (* priority-multiplier (->> state
                                (:items)
                                (map :floor)
                                (filter #(= % top-floor))
                                (count))))))

(defn add-to-priority-map
  ([states] (add-to-priority-map (priority-map) states))
  ([initial-map states]
    (->> states
      (map (fn [state] [state (priority state)]))
      (into initial-map))))

(defn solve [initial-state]
  (loop [state initial-state
         states-to-process (add-to-priority-map (next-states initial-state))
         visited (into #{initial-state} (map item-floors (next-states initial-state)))]
    (if (done? state)
      state
      (let [next-state (first (peek states-to-process))
            new-states (->> next-state
                         (next-states)
                         (remove #(contains? visited (item-floors %))))]
        (recur next-state
               (add-to-priority-map (pop states-to-process) new-states)
               (into visited (map item-floors new-states)))))))

(defn create-items-for-part-2 []
  [{:element "dilithium", :type :generator, :floor 0}
   {:element "dilithium", :type :microchip, :floor 0}
   {:element "elerium", :type :generator, :floor 0}
   {:element "elerium", :type :microchip, :floor 0}])

(defn add-items-for-part-2 [state]
  (update state :items #(into % (create-items-for-part-2))))

(defn run [file]
  (->> file
    (read-state)
    (add-items-for-part-2)
    (solve)
    (:moves)))
