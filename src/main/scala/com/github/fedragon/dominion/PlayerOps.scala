package com.github.fedragon.dominion

import com.github.fedragon.dominion.KingdomCards._

trait PlayerOps {

  def playAction(a: Action)(p: Player)(g: Game): Game =
    a match {
      case Cellar =>
        // Discard N cards, draw N cards, +1 action
        val (discarded, newHand) = p.hand.partition(c => p.strategy.whatToDiscard(p.hand).contains(c))
        val p2 = p.copy(hand = newHand, discarded = p.discarded ++ discarded)
        g.update(p2.drawsN(discarded.size).withBonus(Turn(1, 0, 0)))
      case Market =>
        // Draw 1 card, +1 action, +1 buy, +1 coin
        g.update(p.draws.withBonus(Turn(1, 1, 1)))
      case Mine =>
        // Trash 1 treasure card, get 1 whose cost is +3
        val g3 = for {
          treasure <- pickTreasure(p.hand)
          (newTreasure, g2) <- g.trash(treasure).pick(treasureByCost(treasure.cost.value + 3))
        } yield {
          g2.update(withPlayer(p.discard(treasure)) { p2 =>
            p2.copy(hand = p2.hand.+:(newTreasure))
          })
        }

        g3.getOrElse(g)
      case Smithy =>
        // Draw 3 cards
        g.update(p.drawsN(3))
      case other =>
        throw new UnsupportedOperationException(s"Action not supported: $other")
    }

  def withPlayer(p: Player)(f: Player => Player) = f(p)

  private def pickTreasure(cards: Deck) =
    cards.collect {
      case Treasure(t) => t
    }.headOption

  private def treasureByCost(n: Int) = (c: Card) => c match {
    case Treasure(t) => t.cost.value == n
    case _ => false
  }
}

