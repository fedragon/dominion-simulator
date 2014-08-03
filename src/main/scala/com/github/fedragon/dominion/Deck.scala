package com.github.fedragon.dominion

import scala.util.Random

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
        Some(cards(index) -> cards.patch(index, Vector.empty[Card], 1))
      else None
    }

    def pickN(n: Int)(f: Card => Boolean): Option[(Deck, Deck)] = {
      (0 until n).foldLeft(Option((EmptyDeck, cards))) { (state, _) =>
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

    val onlyTreasures: Treasures = cards.collect {
      case Treasure(t) => t
    }

    val onlyVictories: Victories = cards.collect {
      case Victory(v) => v
    }
  }

}

