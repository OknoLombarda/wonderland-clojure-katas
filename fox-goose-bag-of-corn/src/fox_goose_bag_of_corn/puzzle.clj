(ns fox-goose-bag-of-corn.puzzle)

(def start-pos [[[:fox :goose :corn :you] [:boat] []]])

(defn- safe? [company]
  (letfn [(contains-both? [coll a b]
            (and (some #{a} coll) (some #{b} coll)))]
    (not (when (= 2 (count company))
           (or (contains-both? company :fox :goose)
               (contains-both? company :goose :corn))))))

(defn- pick-passenger [candidates]
  (let [without-me    (remove #{:you} candidates)
        safe-to-pick? (fn [p]
                        (safe? (remove #{p} without-me)))]
    (first (filter safe-to-pick? without-me))))

(defmulti land (fn [_ _ _ direction] direction))

(defmethod land :left
  [lshore [_ _ passenger :as boat] rshore _]
  (let [landed-lshore (concat lshore (rest boat))
        new-passenger (if passenger
                        (first lshore)
                        (pick-passenger lshore))
        after-sail-lshore (remove #{:you new-passenger} landed-lshore)]
    [[landed-lshore [:boat] rshore]
     [after-sail-lshore [:boat :you new-passenger] rshore]]))

(defmethod land :right
  [lshore boat rshore _]
  (let [landed-rshore (concat rshore (rest boat))
        count-on-rshore (count rshore)
        new-passenger (cond
                        (or (= 0 count-on-rshore)
                            (safe? (remove #{:you} landed-rshore)))
                        nil

                        (= count-on-rshore 1)
                        (first rshore))
        new-boat (if new-passenger
                   [:boat :you new-passenger]
                   [:boat :you])
        after-sail-rshore (remove #{:you new-passenger} landed-rshore)
        next-seq [[lshore [:boat] landed-rshore]]]
    (if (empty? lshore)
      next-seq
      (conj next-seq [lshore new-boat after-sail-rshore]))))

(defn river-crossing-plan []
  (loop [[lshore boat rshore] (first start-pos)
         plan                 []
         state                (cycle [:left :right])]
    (if (and (empty? lshore) (= 1 (count boat)))
      plan
      (let [next-seq (land lshore boat rshore (first state))
            next-pos (last next-seq)]
        (recur next-pos (concat plan next-seq) (drop 1 state))))))
