package com.github.fedragon.dominion

import scala.util.Random

sealed trait Card {
  val name: String
  val cost: Coins

  override def toString = name
}

case class Coins(value: Int) extends AnyVal

sealed trait Modifiers

trait Attack extends Modifiers

trait Reaction extends Modifiers

abstract class Action(val name: String, val cost: Coins) extends Card

object Action {
  def unapply(card: Card): Option[Action] = card match {
    case a: Action => Some(a)
    case _ => None
  }
}

case class CardValue(value: Int) extends AnyVal

abstract class Treasure(val name: String, val cost: Coins, val value: CardValue) extends Card

object Treasure {
  def unapply(card: Card): Option[Treasure] = card match {
    case t: Treasure => Some(t)
    case _ => None
  }
}

abstract class Victory(val name: String, val cost: Coins, val value: CardValue) extends Card

case class Deck(cards: Vector[Card]) {

  def ++(cs: Deck): Deck = copy(cards ++ cs.cards)

  def +:(c: Card): Deck = copy(c +: cards)

  def collect[T](f: PartialFunction[Card, T]): Vector[T] = cards.collect(f)

  def contains(c: Card) = cards.contains(c)

  def count(f: Card => Boolean): Int = cards.count(f)

  def draw: Option[(Card, Deck)] = cards.headOption.map(hd => (hd, copy(cards.tail)))

  def exists(f: Card => Boolean): Boolean = cards.exists(f)

  def find(f: Card => Boolean): Option[Card] = cards.find(f)

  def partition(f: Card => Boolean): (Deck, Deck) = {
    val (a, b) = cards.partition(f)
    (copy(a), copy(b))
  }

  def pick(f: Card => Boolean): Option[(Card, Deck)] = {
    val index = cards.indexWhere(f)

    if (cards.isDefinedAt(index))
      Some(cards(index) -> copy(cards.patch(index, Vector.empty[Card], 1)))
    else None
  }

  def shuffle: Deck = copy(Random.shuffle(cards))

  def size: Int = cards.size
}

object Deck {
  val EmptyDeck = Deck(Vector.empty[Card])

  def apply(card: Card*) = new Deck(Vector(card: _*))

  def fillWith(n: Int)(card: Card) = Deck(Vector.fill(n)(card))
}

case class Game(players: Vector[Player], cards: Deck, trashed: Deck) {
  def find(player: Player): Player = players.find(_.name == player.name).get

  def playersExcept(player: Player): Vector[Player] = players.filterNot(_.name == player.name)

  def pick(f: Card => Boolean): Option[(Card, Game)] = {
    cards.pick(f).map {
      case (c, d) => (c, copy(cards = d))
    }
  }

  def trash(card: Card): Game = copy(trashed = card +: trashed)

  def update(p: Player): Game = {
    val index = players.indexWhere(_.name == p.name)
    copy(players = players.updated(index, p))
  }
}
