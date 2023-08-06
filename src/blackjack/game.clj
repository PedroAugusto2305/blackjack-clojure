(ns blackjack.game
  (:require [card-ascii-art.core :as card]))

(defn new-card []
  "Gera uma carta com número entre 1 e 13"
  (inc (rand-int 13)))

; vai calcular os pontos de acordo com as cartas
; J, Q, K = 10 (não 11, 12, 13)
; [A 5 7] = 1 + 5 + 7 (13) ou 11 + 5 + 7 (23)
; A = 11 porém se passar de 21, ele vai valer 1


(defn JQK->10 [card]
  (if (> card 10) 10 card))

(defn A->11 [card]
  (if (= card 1) 11 card))

(defn points-cards [cards]
  (let [cards-without-JQK (map JQK->10 cards)
        cards-with-A11 (map A->11 cards-without-JQK)
        points-with-A-1 (reduce + cards-without-JQK)
        points-with-A-11 (reduce + cards-with-A11)]
    (if (> points-with-A-11 21) points-with-A-1 points-with-A-11)))

(defn player [player-name]
  (let [card1 (new-card)
        card2 (new-card)
        cards [card1 card2]
        points (points-cards cards)]
    {:player-name player-name
     :cards       cards
     :points      points}))

; chamar a função new-card oara gerar a nova carta
; atualizar o vetor cards dentro do player com a nova carta
; calcular os pontos do jogador com o novo vetor de cartas
; retornar esse novo jogador
(defn more-card [player]
  (let [card (new-card)
        cards (conj (:cards player) card)
        new-player (update player :cards conj card)
        points (points-cards cards)]
    (assoc new-player :points points)))

(defn player-decision-continue? [player]
  (println (:player-name player) ": mais carta?")
  (= (read-line) "sim"))

(defn dealer-decision-continue? [player-points dealer]
  (let [dealer-points (:points dealer)]
    (if (> player-points 21) false (<= dealer-points player-points))))

; função game, responsavel por perguntar para o jogador se ele quer mais carta
; caso ele queira mais carta, chamar a função more-card
(defn game [player fn-decision-continue?]
  (if (fn-decision-continue? player)
    (let [player-with-more-cards (more-card player)]
      (card/print-player player-with-more-cards)
      (game player-with-more-cards fn-decision-continue?))
    player))

(defn end-game [player dealer]
  (let [player-points (:points player)
        dealer-points (:points dealer)
        player-name (:player-name player)
        dealer-name (:player-name dealer)
        message (cond
                  (and (> player-points 21) (> dealer-points 21)) "Ambos perderam"
                  (= player-points dealer-points) "Empatou"
                  (> player-points 21) (str dealer-name " ganhou!")
                  (> dealer-points 21) (str player-name " ganhou!")
                  (> player-points dealer-points) (str player-name " ganhou!")
                  (> player-points dealer-points) (str dealer-name " ganhou!"))]
    (card/print-player player)
    (card/print-player dealer)
    (print message)))


(def player-1 (player "Pedro"))
(card/print-player player-1)

(def dealer (player "Dealer"))
(card/print-masked-player dealer)

(def player-after-game (game player-1 player-decision-continue?))
(def dealer-after-game(game dealer (partial dealer-decision-continue? (:points player-after-game))))

(end-game player-after-game dealer-after-game)
