package com.github.fedragon

package object dominion {

  type Cards = Vector[Card]

  val ZeroCards = Vector.empty[Card]

  def fillWith(n: Int)(card: Card) = Vector.fill(n)(card)

  def Cards(card: Card*) = Vector(card:_*)
}
