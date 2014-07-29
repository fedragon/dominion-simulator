package com.github.fedragon.dominion

import Deck._
import KingdomCards._
import VictoryCards._

import scalaz.Scalaz._

trait PlayerOps {
  p: Player =>

  def playAction(a: Action)(g: Game): Game =
    a match {
      case Cellar =>
        // Discard N cards, draw N cards, +1 action
        val (discarded, newHand) = p.hand.partition(c => p.strategy.discardForCellar(p.hand).contains(c))
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
          g2.update(withPlayer(p.discard(treasure)) {
            _.handLens.modify(newTreasure +: _)
          })
        }

        g3.getOrElse(g)
      case Moat =>
        // Draw 2 cards
        g.update(p.drawsN(2))
      case Smithy =>
        // Draw 3 cards
        g.update(p.drawsN(3))
      case Spy =>
        // Draw 1 card, +1 action, every player (attacker included) reveals the top card of his deck and the attacker
        // decides whether to discard it or not: if not discarded, the card goes back on top of the deck.

        // Draw 1 card, +1 action
        val attacker = p.draws.actionsLens.modify(_ + 1).reveals(p.strategy.shouldSpyHolderDiscard)
        val g2 = g.update(attacker)

        // Reveal every victim's top card, maybe discard it
        val ps = g2.victims(p).map(_.reveals(attacker.strategy.shouldSpyVictimDiscard))

        ps.foldLeft(g2)((gn, pn) => gn.update(pn))
      case Thief =>
        // Any other player reveals the top 2 cards from his deck: if they revealed any Treasure card, they trash one of
        // them that you choose. You may gain any or all of these trashed cards. They discard the other revealed cards.

        val revealedCards = g.victims(p).map(_.revealsN(2))

        // TODO too big: split into methods and reorganize code
        revealedCards.foldLeft(g) {
          case (game, rv) =>
            val (revealed, victim) = rv

            val treasures = revealed.onlyTreasures

            val gn = if (treasures.isEmpty) {
              // no treasures: discard both revealed cards
              val victim2 = revealed.foldLeft(victim)((state, card) => state.discardedLens.modify(card +: _))
              game.update(victim2)
            } else {
              val (gainable, discardable) = treasures.partition(p.strategy.holderGainsRevealedTreasure)

              (gainable.toList, discardable.toList) match {
                case (hd :: tl, Nil) =>
                  val updatedAttacker = p.discardedLens.modify(hd +: _)

                  val updatedVictim =
                    if (tl.nonEmpty)
                      victim.discardedLens.modify(tl.head +: _)
                    else victim

                  game.update(updatedAttacker).update(updatedVictim)
                case (Nil, hd :: tl) =>
                  val g2 = game.trash(hd)

                  if (tl.nonEmpty)
                    g2.update(victim.discardedLens.modify(tl.head +: _))
                  else g2
                case (gain :: Nil, disc :: Nil) =>
                  val updatedAttacker = p.discardedLens.modify(gain +: _)
                  val updatedVictim = victim.discardedLens.modify(disc +: _)

                  game.update(updatedAttacker).update(updatedVictim)
                case _ => throw new IllegalStateException("How could this ever happen?!")
              }
            }

            // Discard any revealed non-treasure card
            revealed.diff(treasures).foldLeft(gn)((state, card) => state.update(victim.discardedLens.modify(card +: _)))
        }
      case Witch =>
        // Draw 2 cards, give one curse to all other players
        val g2 = g.update(p.drawsN(2))
        val ps = g2.victims(p).map(_.deckLens.modify(Curse +: _))

        ps.foldLeft(g2)((gn, pn) => gn.update(pn))
      case other =>
        throw new UnsupportedOperationException(s"Action not supported: $other")
    }

  private def pickTreasure(p: Player) = p.hand.onlyTreasures.headOption

  private def treasureByCost(n: Coins) = (c: Card) => c match {
    case Treasure(t) => t.cost === n
    case _ => false
  }
}

