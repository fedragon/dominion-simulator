package com.github.fedragon.dominion

import com.github.fedragon.dominion.KingdomCards._

trait PlayerOps {
  self: Player =>

  def plays(a: Action): Player = {
    validateAction(a).fold(this) { _ =>
      val p = a match {
        case Cellar =>
          // Discard N cards, draw N cards, +1 action
          val (discarded, newHand) = hand.partition(c => strategy.whatToDiscard(hand).contains(c))
          val p = copy(hand = newHand, discarded = this.discarded ++ discarded)
          p.drawN(discarded.size).withBonus(Turn(1, 0, 0))
        case Market =>
          // Draw 1 card, +1 action, +1 buy, +1 coin
          draw.withBonus(Turn(1, 1, 1))
        case Smithy =>
          // Draw 3 cards
          drawN(3)

      }

      afterAction(a)(p)
    }
  }

  private def afterAction(a: Action)(p: Player): Player = {
    val p2 = p.discard(a)
    p2.copy(turn = p2.turn.decrActions(1))
  }

  private def validateAction(a: Action) =
    if (turn.hasActions) hand.find(_ == a)
    else None

}
