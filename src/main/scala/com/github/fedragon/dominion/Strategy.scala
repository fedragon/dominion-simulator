package com.github.fedragon.dominion

import VictoryCards.{Duchy, Estate, Province}

trait CellarStrategy {
  def discardForCellar(cards: Deck): Deck
}

trait SpyStrategy {
  def shouldSpyHolderDiscard(card: Card): Boolean
  def shouldSpyVictimDiscard(card: Card): Boolean
}

trait Strategy extends CellarStrategy with SpyStrategy

class DefaultStrategy extends Strategy {
  // TODO improve
  val DiscardableForCellar = Deck(Estate, Duchy, Province)

  def discardForCellar(cards: Deck): Deck = cards.filter(c => DiscardableForCellar.contains(c))

  def shouldSpyHolderDiscard(card: Card) = false
  def shouldSpyVictimDiscard(card: Card) = true
}

