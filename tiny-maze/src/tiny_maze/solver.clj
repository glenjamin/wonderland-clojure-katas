(ns tiny-maze.solver)

(defn m-get
  "Get the value in a maze. Returns nil if out-of-bounds."
  [maze x y]
  (if (and (>= x 0)
           (>= y 0)
           (>= (count maze) y)
           (>= (count (maze y)) x))
    ((maze y) x)
    nil))

(defn m-set "Set the value in a maze."
  [maze x y v] (assoc-in maze [y x] v))

(defn neighbours
  "Find the coords for cells adjecent to the provided coord."
  [x y]
  (let [moves [[1  0]
               [-1  0]
               [0  1]
               [0 -1]]]
    (map (fn [[dx dy]] [(+ dx x) (+ dy y)]) moves)))

(defn open?
  "Check if a cell is 'open' for passage"
  [maze x y] (some? (#{:S 1 :E} (m-get maze x y))))

(defn end?
  "Check if a cell is the end of the maze"
  [maze x y] (= :E (m-get maze x y)))

(defn start?
  "Check if a cell is a start-cell"
  [maze x y] (= :S (m-get maze x y)))

(defn next-maze [{:keys [maze pos trail visited] :as state}]
  ;; TODO: Implement me
  (assoc state :done true))

(defn find-cell [maze pred]
  (first (for [y     (range (count maze))
               x     (range (count (maze y)))
               :when (pred maze x y)] [x y])))

(defn solve-maze [maze]
  (let [;; Find the start cell
        [start-x start-y :as start-pos] (find-cell maze start?)
        ;; Set is to ':x'
        maze                            (m-set maze start-x start-y :x)
        ;; Create lazy sequence of maze solutions
        steps                           (iterate next-maze {:maze    maze
                                                            :pos     start-pos
                                                            :trail   []
                                                            :done    false
                                                            :visited #{}})]
    ;; find the first solution marked as :done
    (->> steps
         (drop-while #(not (:done %)))
         (first)
         (:maze))))
