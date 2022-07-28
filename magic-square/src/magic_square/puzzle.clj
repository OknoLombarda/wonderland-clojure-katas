(ns magic-square.puzzle)

(def values [1.0 1.5 2.0 2.5 3.0 3.5 4.0 4.5 5.0])

(defn- permutate [coll]
  (if (= 1 (count coll))
    (list coll)
    (for [head coll
          tail (permutate (disj (set coll) head))]
      (cons head tail))))

(defn- sum-rows [m]
  (map #(reduce + %) m))

(defn- sum-cols [m]
  [(reduce + (map first m))
   (reduce + (map second m))
   (reduce + (map last m))])

(defn- sum-diagonals [m]
  [(+ (get-in m [0 0]) (get-in m [1 1]) (get-in m [2 2]))
   (+ (get-in m [2 0]) (get-in m [1 1]) (get-in m [0 2]))])

(defn- magic-square? [square]
  (and
   (= (set (sum-rows square))
      (set (sum-cols square))
      (set (sum-diagonals square)))
   (= 1
      (count (set (sum-rows square)))
      (count (set (sum-cols square)))
      (count (set (sum-diagonals square))))))

(defn- to-vec [c]
  (vec (map vec c)))

(defn magic-square [values]
  (->> (permutate values)
       (map #(partition 3 %))
       (map to-vec)
       (filter magic-square?)
       first))
