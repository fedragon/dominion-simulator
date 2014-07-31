package com.github.fedragon.dominion

import Deck._
import KingdomCards._
import VictoryCards._

import scalaz.Scalaz._

trait PlayerOps extends ThiefOps {
  self: Player =>

  def playAction(a: Action)(g: Game): Game =
    a match {
      case Cellar =>
        // Discard N cards, draw N cards, +1 action
        val (discarded, newHand) = self.hand.partition(c => self.strategy.discardForCellar(self.hand).contains(c))
        self.Logger.info(s"${self.name} discards $discarded")
        val p2 = self.copy(hand = newHand, discarded = self.discarded ++ discarded)

        g.update(p2.drawsN(discarded.size).gainsActions(1))
      case Market =>
        // Draw 1 card, +1 action, +1 buy, +1 coin
        g.update(self.draws.gains(Turn(1, 1, Coins(1))))
      case Militia =>
        // +2 coins, every other player discards cards until they have 3 cards in their hand
        val g2 = g.victims(self).foldLeft(g) { (gn, pn) =>
          val toDiscard = pn.strategy.discardForMilitia(pn.hand)
          gn.update(toDiscard.foldLeft(pn)((player, card) => player.discard(card)))
        }

        g2.update(self.gainsCoins(Coins(2)))
      case Mine =>
        // Trash 1 treasure card and get one whose cost is +3
        val g3 = for {
          treasure <- self.strategy.pickTreasureToTrash(self.hand)
          (newTreasure, g2) <- g.trash(treasure).pick(treasureByCost(treasure.cost + Coins(3)))
        } yield {
          self.Logger.info(s"${self.name} trashes ${treasure.name} and gains ${newTreasure.name}")
          g2.update(withPlayer(self.discard(treasure)) {
            _.handLens.modify(newTreasure +: _)
          })
        }

        g3.getOrElse(g)
      case Moat =>
        // Draw 2 cards
        g.update(self.drawsN(2))
      case Smithy =>
        // Draw 3 cards
        g.update(self.drawsN(3))
      case Spy =>
        // Draw 1 card, +1 action, every player (attacker included) reveals the top card of his deck and the attacker
        // decides whether to discard it or not: if not discarded, the card goes back on top of the deck.

        // Draw 1 card, +1 action
        val attacker = self.draws.gainsActions(1).reveals(self.strategy.spyHolderDiscards)
        val g2 = g.update(attacker)

        // Reveal every victim's top card, maybe discard it
        val ps = g2.victims(self).map(_.reveals(attacker.strategy.spyVictimDiscards))

        ps.foldLeft(g2)((gn, pn) => gn.update(pn))
      case Thief =>
        // Any other player reveals the top 2 cards from his deck: if they revealed any Treasure card, they trash one of
        // them that you choose. You may gain any or all of these trashed cards. They discard the other revealed cards.

        playThief(g)
      case Witch =>
        // Draw 2 cards, give one curse to all other players
        val g2 = g.update(self.drawsN(2))

        g2.victims(self).foldLeft(g2) { (gn, pn) =>
          if (gn.curses > 0)
            gn.drawCurse.update(pn.deckLens.modify(Curse +: _))
          else gn
        }
      case other =>
        throw new UnsupportedOperationException(s"Action not supported: $other")
    }

  private def treasureByCost(n: Coins) = (c: Card) => c match {
    case Treasure(t) => t.cost === n
    case _ => false
  }
}

trait ThiefOps {
  self: Player =>

  def playThief(g: Game): Game = {
    // every victim reveals the top 2 cards from his deck
    val revealedCards = g.victims(self).map(_.revealsN(2))

    revealedCards.foldLeft(g) {
      case (game, rv) =>
        val (revealed, victim) = rv

        val treasures = revealed.onlyTreasures

        self.Logger.info(s"Treasures revealed $treasures")

        val gn = if (treasures.isEmpty) {
          // no treasures: discard both revealed cards
          val victim2 = revealed.foldLeft(victim)((state, card) => state.discardedLens.modify(card +: _))
          self.Logger.info(s"No treasures. ${victim.name} discards revealed cards $revealed")
          game.update(victim2)
        } else {
          val (gainable, discardable) = treasures.partition(self.strategy.holderGainsRevealedTreasure)

          (gainable.toList, discardable.toList) match {
            case (hd :: tl, Nil) =>
              // gain one, discard the other
              onlyGainableTreasures(hd, tl)(victim)(game)
            case (Nil, hd :: tl) =>
              // trash one, discard the other
              onlyDiscardableTreasures(hd, tl)(victim)(game)
            case (gain :: Nil, disc :: Nil) =>
              // gain one, discard one
              oneGainableOneDiscardable(gain, disc)(victim)(game)
            case _ => throw new IllegalStateException("How could self even happen?!")
          }
        }

        // Discard any revealed non-treasure card
        revealed.diff(treasures).foldLeft(gn)((state, card) => state.update(victim.discardedLens.modify(card +: _)))
    }
  }

  private def onlyGainableTreasures(hd: Treasure, tl: List[Treasure])(victim: Player)(game: Game): Game = {
    val updatedAttacker = self.discardedLens.modify(hd +: _)

    self.Logger.info(s"${self.name} gains $hd from ${victim.name}, ${victim.name} discards [$tl]")

    val updatedVictim =
      if (tl.nonEmpty)
        victim.discardedLens.modify(tl.head +: _)
      else victim

    game.update(updatedAttacker).update(updatedVictim)
  }

  private def onlyDiscardableTreasures(hd: Treasure, tl: List[Treasure])(victim: Player)(game: Game): Game = {
    val g2 = game.trash(hd)

    self.Logger.info(s"${victim.name} trashes $hd and discards [$tl]")

    if (tl.nonEmpty)
      g2.update(victim.discardedLens.modify(tl.head +: _))
    else g2
  }

  private def oneGainableOneDiscardable(gain: Treasure, disc: Treasure)(victim: Player)(game: Game): Game = {
    val updatedAttacker = self.discardedLens.modify(gain +: _)
    val updatedVictim = victim.discardedLens.modify(disc +: _)

    self.Logger.info(s"${self.name} gains $gain from ${victim.name}; ${victim.name} discards [$disc]")

    game.update(updatedAttacker).update(updatedVictim)
  }
}

