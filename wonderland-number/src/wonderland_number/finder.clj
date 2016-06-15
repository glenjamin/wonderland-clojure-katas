(ns wonderland-number.finder)

(defn wonderland-number? [n]
  (letfn [(same-digits? [n1]
            (let [s1 (set (str n1))
                  s2 (set (str n))]
              (= s1 s2)))]
    (and (same-digits? (* n 2))
         (same-digits? (* n 3))
         (same-digits? (* n 4))
         (same-digits? (* n 5))
         (same-digits? (* n 6)))))

(defn wonderland-number []
  (->> (range 100000 1000000)
       (filter wonderland-number?)
       (first)))
