package com.github.fedragon.dominion

import scala.util.Random

object Dominion {

  sealed trait Card {
    val name: String
    val cost: Coins

    override def toString = name
  }

  case class Coins(value: Int) extends AnyVal

  abstract class Action(val name: String, val cost: Coins) extends Card {
    def play(p: Player): Player
  }

  case class CardValue(value: Int) extends AnyVal

  abstract class Treasure(val name: String, val cost: Coins, val value: CardValue) extends Card

  abstract class Victory(val name: String, val cost: Coins, val value: CardValue) extends Card

  case class Deck private[dominion](cards: Vector[Card]) {
    def draw: Option[(Card, Deck)] =
      if (cards.isEmpty) None
      else Some((cards.head, copy(cards.tail)))

    def insert(cs: Vector[Card]) = copy(cards ++ cs)

    def shuffle: Deck = copy(Random.shuffle(cards))
  }

  case class Player(name: String,
                    hand: Vector[Card] = Vector.empty,
                    discarded: Vector[Card] = Vector.empty,
                    deck: Deck,
                    turn: Turn = Turn(actions = 1, buys = 1, coins = 0)) {

    def addBonuses(t: Turn): Player = copy(turn = turn + t)

    def canBuy(that: Card): Boolean = {
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

    def plays: Player =
      if (turn.hasActions)
        hand.find { case (_: Action) => true; case _ => false} match {
          case Some(action: Action) => action.play(this).copy(turn = turn.decrActions(1))
          case _ => this
        }
      else this

  }

  case class Turn(actions: Int, buys: Int, coins: Int) {
    def decrActions(by: Int) = copy(actions - by, buys)
    def incrActions(by: Int) = copy(actions + by, buys)
    def hasActions = actions > 0

    def decrBuys(by: Int) = copy(actions, buys - by)
    def incrBuys(by: Int) = copy(actions, buys + by)
    def hasBuys = buys > 0

    def +(that: Turn) =
      copy(
        actions = actions + that.actions,
        buys = buys + that.buys,
        coins = coins + that.coins
      )
  }

}

