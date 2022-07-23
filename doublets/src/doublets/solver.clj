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
      (->> candidates
           (map #(chain
                  (conj chain-so-far %)
                  (remove #{%} words)
                  goal))
           lazy-seq
           (filter seq)
           (#(nth % 0 []))))))

(defn doublets [w1 w2]
  (let [length (count w1)
        words (filter #(= length (count %)) words)]
    (chain [w1] words w2)))
