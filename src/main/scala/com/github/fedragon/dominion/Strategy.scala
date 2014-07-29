package com.github.fedragon.dominion

import Deck._

trait Strategy extends CellarStrategy with SpyStrategy

trait CellarStrategy {
  def whatToDiscard(cards: Deck): Deck
}

trait SpyStrategy {
  def shouldSpyHolderDiscard(card: Card): Boolean
  def shouldSpyVictimDiscard(card: Card): Boolean
}

class DefaultStrategy extends Strategy {
  // TODO improve

  def whatToDiscard(cards: Deck): Deck =
    cards.draw.map {
      case (card, _) => Deck(card)
    }.getOrElse(EmptyDeck)

  def shouldSpyHolderDiscard(card: Card) = false
  def shouldSpyVictimDiscard(card: Card) = true
}

