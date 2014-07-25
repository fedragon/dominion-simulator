package com.github.fedragon.dominion

import scala.util.Random

object Dominion {

  sealed trait Card {
    val name: String
    val typ: CardType
    val cost: Coins

    override def toString = name
  }

  type Cards = Vector[Card]

  sealed trait CardType

  case object Action extends CardType

  case object Treasure extends CardType

  case object Victory extends CardType

  case class CardValue(value: Int) extends AnyVal

  case class Coins(value: Int) extends AnyVal

  abstract class Action(val name: String, val cost: Coins) extends Card {
    val typ = Action

    def play(p: Player): Player
  }

  case object Smithy extends Action("Smithy", cost = Coins(3)) {
    def play(p: Player): Player = p.draw.draw.draw // Draw 3 cards
  }

  abstract class Treasure(val name: String, val cost: Coins, val value: CardValue) extends Card {
    val typ = Treasure
  }

  case object Copper extends Treasure("Copper", cost = Coins(0), value = CardValue(1))

  abstract class Victory(val name: String, val cost: Coins, val value: CardValue) extends Card {
    val typ = Victory
  }

  case object Estate extends Victory("Estate", cost = Coins(2), value = CardValue(1))

  case class Deck private[dominion](cards: Cards) {
    def draw: Option[(Card, Deck)] =
      if (cards.isEmpty) None
      else Some((cards.head, copy(cards.tail)))

    def insert(cs: Cards) = Deck(cards ++ cs)

    def shuffle: Deck = {
      println("Shuffling")
      copy(Random.shuffle(cards))
    }
  }

  case class Player(name: String, hand: Cards, discarded: Cards, deck: Deck) {

    def this(name: String, deck: Deck) = this(name, Vector.empty, Vector.empty, deck)

    def canBuy(that: Card) = {
      val coins = hand.count {
        case _: Treasure => true
        case _ => false
      }

      coins >= that.cost.value
    }

    def draw: Player = {
      deck.draw match {
        case Some((card, newDeck)) =>
          copy(hand = card +: hand, deck = newDeck)
        case None =>
          val (card, newDeck) = deck.insert(discarded).shuffle.draw.get
          copy(hand = card +: hand, discarded = Vector.empty, deck = newDeck)
      }
    }

    def discard: Player = copy(hand = Vector.empty, discarded = discarded ++ hand)
  }

}
