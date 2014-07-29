package com.github.fedragon.dominion

import Deck._
import scalaz.Scalaz._

trait Strategy {
  def buyPreference(cards: Deck): Deck
  def sortByPreference(actions: Actions): Actions

  // Militia specific action
  def whatToDiscard(cards: Deck): Deck

  // Spy specific actions
  def shouldIDiscard(card: Card): Boolean
  def shouldVictimDiscard(card: Card): Boolean
}

class DefaultStrategy extends Strategy {
  // TODO improve

  def sortByPreference(actions: Actions): Actions = actions.sortWith(_.cost > _.cost)

  def buyPreference(cards: Deck): Deck = cards.sortWith(_.cost > _.cost)

  override def whatToDiscard(cards: Deck): Deck =
    cards.draw.map {
      case (card, _) => Deck(card)
    }.getOrElse(EmptyDeck)

  override def shouldIDiscard(card: Card) = false
  override def shouldVictimDiscard(card: Card) = true
}

