package com.github.fedragon.dominion

import scala.util.Random

sealed trait Card {
  val name: String
  val cost: Coins

  override def toString = name
}

case class Coins(value: Int) extends AnyVal

abstract class Action(val name: String, val cost: Coins) extends Card

case class CardValue(value: Int) extends AnyVal

abstract class Treasure(val name: String, val cost: Coins, val value: CardValue) extends Card

abstract class Victory(val name: String, val cost: Coins, val value: CardValue) extends Card

case class Deck(cards: Vector[Card]) {

  def ++(cs: Deck) = copy(cards ++ cs.cards)

  def +:(c: Card) = copy(c +: cards)

  def contains(c: Card) = cards.contains(c)

  def count(f: Card => Boolean) = cards.count(f)

  def draw: Option[(Card, Deck)] = cards.headOption.map(hd => (hd, copy(cards.tail)))

  def find(f: Card => Boolean): Option[Card] = cards.find(f)

  def partition(f: Card => Boolean) = {
    val (a, b) = cards.partition(f)
    (copy(a), copy(b))
  }

  def pick(c: Card): Deck = {
    val index = cards.indexOf(c)

    val newDeck =
      if (cards.isDefinedAt(index))
        cards.patch(index, Vector.empty[Card], 1)
      else cards

    copy(cards = newDeck)
  }

  def shuffle: Deck = copy(Random.shuffle(cards))

  def size = cards.size
}

object Deck {
  val EmptyDeck = Deck(Vector.empty[Card])

  def apply(card: Card*) = new Deck(Vector(card:_*))

  def fillWith(n: Int)(card: Card) = Deck(Vector.fill(n)(card))
}

