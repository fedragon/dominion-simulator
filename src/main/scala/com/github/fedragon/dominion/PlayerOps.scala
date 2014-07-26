package com.github.fedragon.dominion

import com.github.fedragon.dominion.KingdomCards._

trait PlayerOps {

  def playAction(a: Action)(p: Player): Player =
    a match {
      case Cellar =>
        // Discard N cards, draw N cards, +1 action
        val (discarded, newHand) = p.hand.partition(c => p.strategy.whatToDiscard(p.hand).contains(c))
        val p2 = p.copy(hand = newHand, discarded = p.discarded ++ discarded)
        p2.drawsN(discarded.size).withBonus(Turn(1, 0, 0))
      case Market =>
        // Draw 1 card, +1 action, +1 buy, +1 coin
        p.draws.withBonus(Turn(1, 1, 1))
      case Smithy =>
        // Draw 3 cards
        p.drawsN(3)
    }
}

