(ns card-game-war.game)

(def suits [:spade :club :diamond :heart])
(def ranks [2 3 4 5 6 7 8 9 10 :jack :queen :king :ace])
(defonce cards
  (for [suit suits
        rank ranks]
    [suit rank]))
(defonce card-values
  (->> ranks
       (map-indexed (fn [i e] {e i}))
       (reduce merge {})))

(defn play-round
  "Returns :card1 if first card is greater in value, :card2 if opposite, or nil
  if cards are equal in value"
  [[_ rank1] [_ rank2]]
  (let [card1-value (get card-values rank1)
        card2-value (get card-values rank2)]
    (cond
      (> card1-value card2-value) :card1
      (< card1-value card2-value) :card2)))

(defn- game [ac1 d1 ac2 d2]
  (let [c1 (first ac1)
        c2 (first ac2)]
    (cond
      (or (and (empty? d1) (empty? d2))
          (and (not c1) (not c2)))
      "It's a draw"

      (or (not c1) (empty? d1)) "Player 2 won"

      (or (not c2) (empty? d2)) "Player 1 won"

      :else
      (case (play-round c1 c2)
        :card1 (recur [(first d1)]
                     (concat (rest d1) ac1 ac2)
                     [(first d2)]
                     (rest d2))
        :card2 (recur [(first d1)]
                     (rest d1)
                     [(first d2)]
                     (concat (rest d2) ac2 ac1))
        nil    (recur (concat (reverse (take 4 d1)) ac1)
                     (drop 4 d1)
                     (concat (reverse (take 4 d2)) ac2)
                     (drop 4 d2))))))

(defn play-game [player1-cards player2-cards]
  (game [(first player1-cards)]
        (rest player1-cards)
        [(first player2-cards)]
        (rest player2-cards)))
