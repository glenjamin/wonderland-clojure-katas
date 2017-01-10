(ns alphabet-cipher.coder)

(defn rotate
  [[head & tail]]
  (concat tail [head]))

(defn all-prefixes [string]
  (map #(take % string) (range 1 (inc (count string)))))

(defn uncycle
  [repeated]
  (->> (all-prefixes repeated)
       (filter #(= repeated
                   (->> % cycle (take (count repeated)) (apply str))))
       (first)
       (apply str)))

(def alpha (->> (range 0 26)
                (map (partial + 97))
                (map char)))

(def alphabets (take 26 (iterate rotate alpha)))

(def forwards (for [row alphabets] [(first row) (map vector alpha row)]))
(def backwards (for [row alphabets] [(first row) (map vector row alpha)]))

(def to-map (partial into {}))
(defn tableify [data] (->> data
                          (map #(update-in % [1] to-map))
                          to-map))

(def table-forwards (tableify forwards))
(def table-backwards (tableify backwards))

(defn uncode [table keyword message]
  (apply str (map #(get-in table [%1 %2]) (cycle keyword) message)))

(defn encode [keyword message]
  (uncode table-forwards keyword message))

(defn decode [keyword message]
  (uncode table-backwards keyword message))

(defn decipher [cipher message]
  (uncycle (uncode table-backwards message cipher)))

