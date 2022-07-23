(ns doublets.solver
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]))

(defonce words (-> "words.edn"
                   (io/resource)
                   (slurp)
                   (edn/read-string)))

(defn- count-diff [w1 w2]
  (reduce + (map #(if (= %1 %2) 0 1) w1 w2)))

(defn- chain [chain-so-far words goal]
  (let [last (last chain-so-far)
        candidates (filter #(= 1 (count-diff last %)) words)]
    (cond
      (= 1 (count-diff goal last))
      (conj chain-so-far goal)

      (empty? candidates)
      []

      :else
      (nth (filter seq
                   (lazy-seq
                    (map #(chain (conj chain-so-far %)
                                 (remove #{%} words)
                                 goal)
                         candidates)))
           0 []))))

(defn doublets [w1 w2]
  (let [length (count w1)
        words (filter #(and (= length (count %)) #{w1})
                      words)]
    (chain [w1] words w2)))
