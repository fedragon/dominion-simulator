package com.github.fedragon.dominion

import Deck._
import TreasureCards.{Copper, Gold, Silver}
import VictoryCards.{Duchy, Estate, Province}

import scalaz.Scalaz._

class Aggressive extends Strategy {

  val DiscardableForCellar = Deck(Estate, Duchy, Province)
  val DiscardableForMilitia = Deck(Estate, Duchy, Province)

  override def discardDeck(): Boolean = true

  override def discardForCellar(cards: Deck) = cards.filter(c => DiscardableForCellar.contains(c))

  override def discardForMilitia(cards: Deck): Deck =
    if (cards.size <= 3) EmptyDeck
    else cards.take(cards.size - 3)

  override def holderGainsRevealedTreasure(card: Card) = card === Silver || card === Gold

  override def makeGroceriesList(cards: Deck) = cards.filterNot(_ === Curse)

  override def pickCardToGain(cards: Deck)(cost: Coins): Option[Card] =
    cards.sortWith(_.cost > _.cost).filterNot(_.cost > cost + Coins(2)).headOption
  override def pickCardToTrash(cards: Deck): Option[(Card, Deck)] = cards.pick(_ === Copper)

  override def pickCardsToTrash(cards: Deck): Option[(Deck, Deck)] = cards.pickN(4)(_ === Curse)

  override def pickTreasureToTrash(cards: Deck) = cards.onlyTreasures.sortWith(_.value < _.value).headOption

  override def selectActionForThroneRoom(cards: Deck): Option[Action] = cards.onlyActions.headOption

  override def selectCardForFeast(cards: Deck): Card = cards.sortWith(_.cost > _.cost).head

  override def selectCardForWorkshop(cards: Deck): Card = cards.sortWith(_.cost > _.cost).head

  override def selectNextActions(actions: Actions) = actions.sortWith(_.cost > _.cost)

  override def spyHolderDiscards(card: Card) = false
  override def spyVictimDiscards(card: Card) = true
}
