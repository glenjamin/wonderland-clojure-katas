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
  (let [moves [[ 1  0]
               [-1  0]
               [ 0  1]
               [ 0 -1]]]
    (map (fn [[dx dy]] [(+ dx x) (+ dy y)]) moves)))

(defn open?
  "Check if a cell is 'open' for passage"
  [maze x y] (some? (#{:S 1 :E} (m-get maze x y))))

(defn end?
  "Check if a cell is the end of the maze"
  [maze x y] (= :E (m-get maze x y)))

(defn do-solve-maze [{:keys [maze pos trail] :as state}]
  (if (:done state)
    state
    (let [lookup     (fn [[x y]] [x y (m-get maze x y)])
          filter-fn  (fn [[_ _ v]] (not (nil? v)))
          neighbours (filter filter-fn (map lookup (apply neighbours pos)))]
      (assoc state :neighbours neighbours))))


(defn solve-maze [maze] )
