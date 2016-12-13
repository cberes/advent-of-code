(ns advent-of-code.day11
  (:use [advent-of-code.util :only [read-lines enumerate]]))

(def elevator-start 0)

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
      (mapcat identity [(map (fn [g] {:element g, :type :generator, :floor floor}) generators)
                        (map (fn [m] {:element m, :type :microchip, :floor floor}) microchips)]))))

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

; TODO could refactor next 2 methods to share code
(defn single-floor-change [state op]
  (let [indices (indices-to-change state)]
    (map (fn [index]
           (-> state
             (update :moves inc)
             (update :elevator op)
             (assoc :items (update (:items state) index #(update % :floor op)))))
         indices)))

(defn double-floor-change [state op]
  (let [update-floor #(update % :floor op)
        single-indices (indices-to-change state)
        indices (for [a single-indices b single-indices :when (< a b)] [a b])]
    (map (fn [[a b]]
           (-> state
             (update :moves inc)
             (update :elevator op)
             (assoc :items (update (update (:items state) a update-floor) b update-floor))))
         indices)))

(defn next-states [state]
  (filter valid?
          (concat (single-floor-change state inc)
                  (double-floor-change state inc)
                  (single-floor-change state dec)
                  (double-floor-change state dec))))

(defn done? [state]
  (every? #(= (:floor %) (dec (:floors state))) (:items state)))

(defn solve [initial-state]
  (loop [state initial-state
         states-to-process (next-states initial-state)
         visited (into #{initial-state} (map item-floors states-to-process))]
    (if (done? state)
      state
      (let [new-states (->> state
                         (next-states)
                         (remove #(contains? visited (item-floors %))))]
        (recur (first states-to-process)
               (doall (concat (rest states-to-process) new-states))
               (into visited (map item-floors new-states)))))))

(defn run [file]
  (->> file
    (read-state)
    (solve)
    (:moves)))
