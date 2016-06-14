(ns fox-goose-bag-of-corn.puzzle)

(def start-pos [[[:fox :goose :corn :you] [:boat] []]])

(defn allowed? [items]
  (let [items (set items)]
    (or (#{:you} items)
        (not (clojure.set/subset? #{:fox :goose} items))
        (not (clojure.set/subset? #{:corn :goose} items)))))

(defn river-crossing-plan []
  start-pos)
