package com.github.fedragon

package object dominion {

  type Actions = Vector[Action]

  type Deck = Vector[Card]

  type Treasures = Vector[Treasure]

  def withPlayer[T](p: Player)(f: Player => T) = f(p)
}

