(ns tiny-maze.solver)

(defn value-at [maze x y]
  ((maze y) x))

(defn open? [maze x y]
  (some? (#{:S 1 :E} (value-at maze x y))))

(defn do-solve-maze [ctx maze])

(defn find-start [maze]
  (let [coords    (for [y (range (count maze))
                        x (range (count (maze y)))] [x y])
        predicate (fn [[x y]] (= :S (value-at maze x y)))]
    (first (filter predicate coords))))

(defn solve-maze [maze]
  (do-solve-maze {:visited #{}
                  :current (find-start maze)}
                 maze))
