package com.github.fedragon.dominion

import KingdomCards._
import VictoryCards._

trait PlayerOps {

  def playAction(a: Action)(p: Player)(g: Game): Game =
    a match {
      case Cellar =>
        // Discard N cards, draw N cards, +1 action
        val (discarded, newHand) = p.hand.partition(c => p.whatToDiscard(p.hand).contains(c))
        val p2 = p.copy(hand = newHand, discarded = p.discarded ++ discarded)

        g.update(p2.drawsN(discarded.size).actionsLens modify (_ + 1))
      case Market =>
        // Draw 1 card, +1 action, +1 buy, +1 coin
        g.update(p.draws.turnLens modify (_ + Turn(1, 1, Coins(1))))
      case Mine =>
        // Trash 1 treasure card and get one whose cost is +3
        val g3 = for {
          treasure <- pickTreasure(p)
          (newTreasure, g2) <- g.trash(treasure).pick(treasureByCost(treasure.cost + Coins(3)))
        } yield {
          g2.update(withPlayer(p.discard(treasure))(_.handLens.modify(newTreasure +: _)))
        }

        g3.getOrElse(g)
      case Moat =>
        // Draw 2 cards
        g.update(p.drawsN(2))
      case Smithy =>
        // Draw 3 cards
        g.update(p.drawsN(3))
      case Witch =>
        // Draw 2 cards, give one curse to all other players
        val g2 = g.update(p.drawsN(2))
        val ps = g2.victims(p).map(_.deckLens.modify(Curse +: _))

        ps.foldLeft(g2)((gn, pn) => gn.update(pn))
      case other =>
        throw new UnsupportedOperationException(s"Action not supported: $other")
    }

  private def pickTreasure(p: Player) = p.treasures.headOption

  private def treasureByCost(n: Coins) = (c: Card) => c match {
    case Treasure(t) => t.cost == n
    case _ => false
  }
}

