(ns wonderland-number.finder)

(defn same-digits? [n1 n2]
  (let [n1-chars (group-by identity (str n1))
        n2-chars (group-by identity (str n2))]
    (= n1-chars n2-chars)))

(defn wonderland-number? [num]
  (let [products (for [m (range 2 7)]
                   (* m num))]
    (every? #(same-digits? num %) products)))

(defn wonderland-number []
  (let [upper-limit (int (java.lang.Math/ceil (/ 1000000.0 6)))
        candidates (range 100000 upper-limit)]
    (first (filter wonderland-number? candidates))))
