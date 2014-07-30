package com.github.fedragon.dominion

import TreasureCards.{Gold, Silver}
import VictoryCards.{Duchy, Estate, Province}

trait CellarStrategy {
  def discardForCellar(cards: Deck): Deck
}

trait SpyStrategy {
  def spyHolderDiscards(card: Card): Boolean
  def spyVictimDiscards(card: Card): Boolean
}

trait ThiefStrategy {
  def holderGainsRevealedTreasure(card: Card): Boolean
}

trait Strategy extends CellarStrategy with SpyStrategy with ThiefStrategy

class DefaultStrategy extends Strategy {
  // TODO improve
  val DiscardableForCellar = Deck(Estate, Duchy, Province)

  def discardForCellar(cards: Deck): Deck = cards.filter(c => DiscardableForCellar.contains(c))

  def spyHolderDiscards(card: Card) = false
  def spyVictimDiscards(card: Card) = true

  def holderGainsRevealedTreasure(card: Card) = card == Silver || card == Gold
}

