package com.github.fedragon.dominion

import scala.util.Random

object Dominion {

  sealed trait Card {
    val name: String
    val cost: Coins

    override def toString = name
  }

  abstract class Action(val name: String, val cost: Coins) extends Card {
    def play(p: Player): Player
  }

  type Cards = Vector[Card]

  case class CardValue(value: Int) extends AnyVal

  case class Coins(value: Int) extends AnyVal

  abstract class Treasure(val name: String, val cost: Coins, val value: CardValue) extends Card

  abstract class Victory(val name: String, val cost: Coins, val value: CardValue) extends Card

  case class Deck private[dominion](cards: Cards) {
    def draw: Option[(Card, Deck)] =
      if (cards.isEmpty) None
      else Some((cards.head, copy(cards.tail)))

    def insert(cs: Cards) = copy(cards ++ cs)

    def shuffle: Deck = copy(Random.shuffle(cards))
  }

  case class Player(name: String,
                    hand: Cards = Vector.empty,
                    discarded: Cards = Vector.empty,
                    deck: Deck,
                    turn: Turn = Turn(1, 1)) {

    def canBuy(that: Card) = {
      val coins = hand.count { case (_: Treasure) => true; case _ => false}
      coins >= that.cost.value
    }

    def draw: Player =
      deck.draw match {
        case Some((card, newDeck)) =>
          copy(hand = card +: hand, deck = newDeck)
        case None =>
          val (card, newDeck) = deck.insert(discarded).shuffle.draw.get
          copy(hand = card +: hand, discarded = Vector.empty, deck = newDeck)
      }

    def discard: Player = copy(hand = Vector.empty, discarded = discarded ++ hand)

    def playAction: Player =
      if (turn.hasActions)
        hand.find { case (_: Treasure) => true; case _ => false} match {
          case Some(action: Action) => action.play(this).copy(turn = turn.decrActions(1))
          case _ => this
        }
      else this

  }

  case class Turn(actions: Int, buys: Int) {
    def decrActions(by: Int) = copy(actions - by, buys)
    def incrActions(by: Int) = copy(actions + by, buys)
    def hasActions = actions > 0

    def decrBuys(by: Int) = copy(actions, buys - by)
    def incrBuys(by: Int) = copy(actions, buys + by)
    def hasBuys = buys > 0
  }

}

