package com.github.fedragon

import scalaz.{Equal, Order}

package object dominion {

  type Actions = Vector[Action]

  type Curses = Vector[Curse.type]

  type Deck = Vector[Card]

  type Treasures = Vector[Treasure]

  type Victories = Vector[Victory]

  def withPlayer[T](p: Player)(f: Player => T) = f(p)

  implicit val CardEqual: Equal[Card] = Equal.equalA
  implicit val CoinsEqual: Equal[Coins] = Equal.equalA
  implicit val CoinsOrdering: Ordering[Coins] = Ordering.by(_.value)
  implicit val CoinsOrder: Order[Coins] = Order.fromScalaOrdering
}

