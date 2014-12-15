package com.github.fedragon.dominion

import scala.util.Random
import scalaz.Scalaz._

object Deck {
  val EmptyDeck = Vector.empty[Card]

  def apply(card: Card*) = Vector(card: _*)

  def fillWith(n: Int)(card: Card) = Vector.fill(n)(card)

  implicit class ImplicitDeck(cards: Deck) {

    def draw: Option[(Card, Deck)] = cards.headOption.map(hd => (hd, cards.tail))

    def shuffle: Deck = Random.shuffle(cards)

    def pick(f: Card => Boolean): Option[(Card, Deck)] = {
      val index = cards.indexWhere(f)

      if (cards.isDefinedAt(index))
        (cards(index) -> cards.patch(index, EmptyDeck, 1)).some
      else None
    }

    def pickN(n: Int)(f: Card => Boolean): Option[(Deck, Deck)] = {
      (0 until n).foldLeft((EmptyDeck, cards).some) { (state, _) =>
        state.flatMap {
          case (picked, remaining) =>
            remaining.pick(f).collect {
              case (card, others) => (card +: picked, others)
            }
        }.orElse(state)
      }
    }

    val onlyActions: Actions = cards.collect {
      case Action(t) => t
    }

    val onlyCurses: Curses = cards.collect {
      case Curse(t) => t
    }

    val onlyTreasures: Treasures = cards.collect {
      case Treasure(t) => t
    }

    val onlyVictories: Victories = cards.collect {
      case Victory(v) => v
    }
  }
}

