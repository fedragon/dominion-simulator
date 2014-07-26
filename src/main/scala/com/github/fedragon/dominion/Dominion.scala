package com.github.fedragon.dominion

import scala.util.Random

sealed trait Card {
  val name: String
  val cost: Coins

  override def toString = name
}

case class Coins(value: Int) extends AnyVal

abstract class Action(val name: String, val cost: Coins) extends Card {
  def play(p: Player): Player
}

case class CardValue(value: Int) extends AnyVal

abstract class Treasure(val name: String, val cost: Coins, val value: CardValue) extends Card

abstract class Victory(val name: String, val cost: Coins, val value: CardValue) extends Card

case class Deck private[dominion](cards: Cards) {
  def draw: Option[(Card, Deck)] =
    if (cards.isEmpty) None
    else Some((cards.head, copy(cards.tail)))

  def insert(cs: Cards) = copy(cards ++ cs)

  def shuffle: Deck = copy(Random.shuffle(cards))
}

