package com.github.fedragon.dominion

import TreasureCards.{Gold, Silver}
import VictoryCards.{Curse, Duchy, Estate, Province}

trait TurnStrategy {
  def selectNextActions(hand: Actions): Actions
  def makeGroceriesList(hand: Deck): Deck
}

trait CellarStrategy {
  def discardForCellar(cards: Deck): Deck
}

trait MilitiaStrategy {
  def discardForMilitia(cards: Deck): Deck
}

trait MineStrategy {
  def pickTreasureToTrash(hand: Deck): Option[Treasure]
}

trait SpyStrategy {
  def spyHolderDiscards(card: Card): Boolean
  def spyVictimDiscards(card: Card): Boolean
}

trait ThiefStrategy {
  def holderGainsRevealedTreasure(card: Card): Boolean
}

trait Strategy extends TurnStrategy
with CellarStrategy
with MilitiaStrategy
with MineStrategy
with SpyStrategy
with ThiefStrategy

class DefaultStrategy extends Strategy {

  import Deck._
  import scalaz.Scalaz._

  val DiscardableForCellar = Deck(Estate, Duchy, Province)
  val DiscardableForMilitia = Deck(Estate, Duchy, Province)

  override def discardForCellar(cards: Deck) = cards.filter(c => DiscardableForCellar.contains(c))

  override def discardForMilitia(cards: Deck): Deck =
    if (cards.size <= 3) EmptyDeck
    else cards.take(cards.size - 3)

  override def holderGainsRevealedTreasure(card: Card) = card == Silver || card == Gold

  override def makeGroceriesList(cards: Deck) = cards.filterNot(_ == Curse)

  override def pickTreasureToTrash(hand: Deck) = hand.onlyTreasures.headOption

  override def selectNextActions(actions: Actions) = actions.sortWith(_.cost > _.cost)

  override def spyHolderDiscards(card: Card) = false
  override def spyVictimDiscards(card: Card) = true
}

