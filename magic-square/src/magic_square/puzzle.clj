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

(defn magic-square [values]
  (let [permutations (comb/permutations values)
        pred (fn [values] (let [sums (calc-square-sums values)]
                            (= 1 (count (distinct sums)))))
        solution (first (drop-while (complement pred) permutations))]
    (vec (map vec (partition 3 solution)))))

