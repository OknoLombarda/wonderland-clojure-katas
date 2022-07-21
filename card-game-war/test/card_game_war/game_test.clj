(ns card-game-war.game-test
  (:require [clojure.test :refer [deftest testing is]]
            [card-game-war.game :refer [play-round play-game cards suits]]))

(deftest test-play-round
  (testing "the highest rank wins the cards in the round"
    (doseq [suit1 suits rank1 (range 2 6) suit2 suits rank2 (range 6 11)]
      (is (= :card2 (play-round [suit1 rank1] [suit2 rank2])))))
  (testing "queens are higher rank than jacks"
    (doseq [suit1 suits suit2 suits]
      (is (= :card1 (play-round [suit1 :queen] [suit2 :jack])))))
  (testing "kings are higher rank than queens"
    (doseq [suit1 suits suit2 suits]
      (is (= :card1 (play-round [suit1 :king] [suit2 :queen])))))
  (testing "aces are higher rank than kings"
    (doseq [suit1 suits suit2 suits]
      (is (= :card1 (play-round [suit1 :ace] [suit2 :king]))))))

(deftest test-play-game
  (testing "the player loses when they run out of cards"
    (let [cardss (shuffle cards)]
      (play-game (take 26 cardss) (drop 26 cardss)))))
