(ns magic-square.puzzle
  (:require [clojure.math.combinatorics :as comb]))

(def values [1.0 1.5 2.0 2.5 3.0 3.5 4.0 4.5 5.0])

(defn values->rows [values] (partition 3 values))
(defn values->cols [values]
  (let [rows (values->rows values)]
    (partition 3 (concat (map #((vec %) 0) rows)
                         (map #((vec %) 1) rows)
                         (map #((vec %) 2) rows)))))
(defn values->diagonals [values]
  (let [[top middle bottom] (vec (map vec (values->rows values)))]
    (partition 3 (concat [(top 0) (middle 1) (bottom 2)]
                         [(top 2) (middle 1) (bottom 0)]))))

(defn calc-square-sums [values]
  (assert (= 9 (count values)))
  (let [row-sums      (map #(reduce + %) (values->rows values))
        col-sums      (map #(reduce + %) (values->cols values))
        diagonal-sums (map #(reduce + %) (values->diagonals values))]
    (concat row-sums col-sums diagonal-sums)))

(defn sum [values] (reduce + values))
(defn make-square [input] (vec (map vec (partition 3 input))))
(defn row-sum [square idx]
  (reduce + (square idx)))
(defn col-sum [square idx]
  (reduce + (map #(get % idx) square)))
(defn diag-sum [square direction]
  (if (= direction :right)
    (reduce + (vector (get-in square [0 0])
                      (get-in square [1 1])
                      (get-in square [2 2])))
    (reduce + (vector (get-in square [0 2])
                      (get-in square [1 1])
                      (get-in square [2 0])))))
(defn solution? [square]
  (let [row0 (row-sum square 0)]
    (and (= row0 (row-sum square 1))
         (= row0 (row-sum square 2))
         (= row0 (col-sum square 0))
         (= row0 (col-sum square 1))
         (= row0 (col-sum square 2))
         (= row0 (diag-sum square :right))
         (= row0 (diag-sum square :left)))))

(defn magic-square
  "Faster implementation. Eagerly returns false when finding a row/col/diag that is not the
  sum of the first row.
  On my machine: 0.4 seconds."
  [values]
  (let [permutations (comb/permutations values)]
    (first
     (drop-while (complement solution?)
           (map make-square permutations)))))

(defn slow-magic-square
  "Original implementation. Slow - fully evaluates each permuation before deciding if
  it's the solution or not.
  On my machine: ~2.5 seconds."
  [values]
  (let [permutations (comb/permutations values)
        pred (fn [values] (let [sums (calc-square-sums values)]
                            (= 1 (count (distinct sums)))))
        solution (first (drop-while (complement pred) permutations))]
    (vec (map vec (partition 3 solution)))))

(defn siamese-magic-square
  "https://en.wikipedia.org/wiki/Siamese_method.
  On my machine: 1.2 ms. We have a winner!"
  [values]
  (assert (= 9 (count values)))
  (let [square (vec (map vec (partition 3 (repeat 9 nil))))]
    (letfn [(get-value [square [x y]] (get-in square [y x]))
            (set-value [square [x y] v] (update-in square [y x] (constantly v)))]
      (loop [pos [1 0]
             square square
             values (sort values)]
        (if (empty? values)
          square ;; solution found
          (let [[head & rest] values
                value-at-pos (get-value square pos)]
            (if value-at-pos
              (recur (map #(mod % 3) (mapv + pos [-1 2]))
                     square
                     values)
              (recur (map #(mod % 3) (mapv + pos [1 -1]))
                     (set-value square pos head)
                     rest))))))))





