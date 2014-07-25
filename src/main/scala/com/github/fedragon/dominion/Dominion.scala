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

  case object Treasure extends CardType

  case object Victory extends CardType

  case class CardValue(value: Int) extends AnyVal

  case class Coins(value: Int) extends AnyVal

  abstract class Treasure(val name: String, val typ: CardType, val cost: Coins, val value: CardValue) extends Card

  case object Copper extends Treasure("Copper", typ = Treasure, cost = Coins(0), value = CardValue(1))

  //  case object Silver extends Treasure("Silver", typ = Treasure, cost = Coins(3), value = CardValue(5))
  //
  //  case object Gold extends Treasure("Gold", typ = Treasure, cost = Coins(6), value = CardValue(8))

  abstract class Victory(val name: String, val typ: CardType, val cost: Coins, val value: CardValue) extends Card

  case object Estate extends Victory("Estate", typ = Victory, cost = Coins(2), value = CardValue(1))

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

  object Deck {
    def apply(): Deck = {
      val cards =
        Vector.fill(60)(Copper) ++
          //          Vector.fill(40)(Silver) ++
          //          Vector.fill(30)(Gold) ++
          Vector.fill(24)(Estate)

      new Deck(cards)
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
