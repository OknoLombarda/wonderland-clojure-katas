(ns tiny-maze.solver "https://en.wikipedia.org/wiki/A*_search_algorithm"
    (:require [clojure.set :refer [union]]))

(defrecord Node [edges coords type])

(defn map-indexes [matrix]
  (loop [x       0
         y       0
         matrix  matrix
         rows    matrix
         columns (first matrix)]
    (cond
      (empty? rows)
      matrix

      (empty? columns)
      (recur 0 (inc y) matrix (rest rows) (first (rest rows)))

      :else
      (recur (inc x)
             y
             (update-in matrix [y x] #(cons % [x y]))
             rows
             (rest columns)))))

(defn val->node-type [val]
  (case val
    :S :start
    0  :path
    1  :wall
    :E :end))

(defn build-unconnected-graph [matrix]
  (vec
   (for [line matrix]
     (->> line
          (map (fn [[v x y]]
                 (map->Node {:type   (val->node-type v)
                             :coords [x y]})))
          vec))))

(defn build-graph [matrix]
  (let [not-wall? (fn [n] (not= :wall (:type n)))
        indexed (map-indexes matrix)
        unconnected (build-unconnected-graph indexed)
        flat (filter not-wall? (flatten unconnected))]
    (map (fn [{[x y] :coords :as node}]
           (let [candidates (for [[dx dy] [[0 1] [0 -1] [1 0] [-1 0]]]
                              (get-in unconnected [(+ y dy) (+ x dx)]))
                 edges (->> candidates
                            (filter #(when % (not-wall? %)))
                            (map :coords))]
             (assoc node :edges edges)))
         flat)))

(defn build-path [came-from end]
  (loop [path []
         current end]
    (if-not (contains? came-from current)
      (cons current path)
      (recur (cons current path) (get came-from current)))))

(defn find-path [maze]
  (let [graph (build-graph maze)
        start (first (filter #(= :start (:type %)) graph))
        calc-g-score (fn [node g-score]
                       (let [s (get g-score node :inf)]
                         (if (= :inf s)
                           :inf
                           (+ 1 s))))
        get-neighbors (fn [{:keys [edges]}]
                        (filter #((set edges) (:coords %)) graph))
        lt (fn [a b]
             (and (not= :inf a) (or (= :inf b) (< a b))))]
    (loop [f-score {start 1}
           open-set (sorted-set-by (comparator #(get f-score %)) start)
           current start
           came-from {}
           g-score {start 0}]
      (if (= :end (:type current))
        (build-path came-from current)
        (let [neighbors (for [neighbor (get-neighbors current)
                              :let [tentative-g-score (calc-g-score current
                                                                    g-score)]
                              :when (lt tentative-g-score
                                        (get g-score neighbor :inf))]
                          [neighbor tentative-g-score])
              nv (reduce (fn [r [n gs]]
                           (-> r
                               (update :open-set conj n)
                               (update :came-from assoc n current)
                               (update :g-score assoc n gs)
                               (update :f-score assoc n (+ 1 gs))))
                         {:open-set #{}
                          :came-from {}
                          :g-score {}
                          :f-score {}}
                         neighbors)
              nos (->> open-set
                       (remove #{current})
                       (union (:open-set nv)))]
          (recur
           (merge f-score (:f-score nv))
           nos
           (first nos)
           (merge came-from (:came-from nv))
           (merge g-score (:g-score nv))))))))

(defn solve-maze [maze]
  (let [path (map :coords (find-path maze))]
    (reduce (fn [m [x y]] (assoc-in m [y x] :x))
            maze
            path)))
