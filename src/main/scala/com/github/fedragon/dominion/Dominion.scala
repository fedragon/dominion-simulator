package com.github.fedragon.dominion

object Dominion {

  sealed trait Card {
    val name: String
    val typ: CardType
    val cost: Coins

    override def toString = name
  }

  sealed trait CardType
  object Treasure extends CardType

  case class CardValue(value: Int) extends AnyVal
  case class Coins(value: Int) extends AnyVal

  abstract class Treasure(val name: String, val typ: CardType, val cost: Coins, val value: CardValue) extends Card
  object Copper extends Treasure("Copper", typ = Treasure, cost = Coins(0), value = CardValue(1))
  object Silver extends Treasure("Silver", typ = Treasure, cost = Coins(3), value = CardValue(5))
  object Gold extends Treasure("Gold", typ = Treasure, cost = Coins(6), value = CardValue(8))

  case class Deck private(cards: Map[Card, Int]) {

    def -=(card: Card, amount: Int) = {
      val newAmount = cards.get(card).fold(0)(_ - amount)
      new Deck(cards.updated(card, newAmount))
    }

  }

  object Deck {
    def apply(): Deck = {
      val cards = Map[Card, Int](
        Copper -> 60,
        Silver -> 40,
        Gold -> 30
      )

      new Deck(cards)
    }
  }
}
