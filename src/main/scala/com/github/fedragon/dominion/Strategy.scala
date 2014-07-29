package com.github.fedragon.dominion

import TreasureCards.{Gold, Silver}
import VictoryCards.{Duchy, Estate, Province}

trait CellarStrategy {
  def discardForCellar(cards: Deck): Deck
}

trait SpyStrategy {
  def shouldSpyHolderDiscard(card: Card): Boolean
  def shouldSpyVictimDiscard(card: Card): Boolean
}

trait ThiefStrategy {
  def holderGainsRevealedTreasure(card: Card): Boolean
}

trait Strategy extends CellarStrategy with SpyStrategy with ThiefStrategy

class DefaultStrategy extends Strategy {
  // TODO improve
  val DiscardableForCellar = Deck(Estate, Duchy, Province)

  def discardForCellar(cards: Deck): Deck = cards.filter(c => DiscardableForCellar.contains(c))

  def shouldSpyHolderDiscard(card: Card) = false
  def shouldSpyVictimDiscard(card: Card) = true

  def holderGainsRevealedTreasure(card: Card) = card == Silver || card == Gold
}

