(ns advent-of-code.core
  (:gen-class))

(defn load-ns [day]
  (let [ns (symbol (str "advent-of-code.day" day))]
    (require ns)
    (find-ns ns)))

(defn run-task [[day file]]
  ((ns-resolve (load-ns day) 'run) file))

(defn -main [& args]
  (println (run-task args)))
