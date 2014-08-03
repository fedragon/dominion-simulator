package com.github.fedragon.dominion

import Deck._
import ActionCards._
import TreasureCards.{Copper, Silver}
import VictoryCards._

import scalaz.Scalaz._

trait PlayerOps extends ThiefOps {
  self: Player =>

  def playAction(a: Action)(g: Game): Game =
    a match {
      case Bureaucrat =>
        // Gain 1 silver, victims reveal a Victory from their hand and put it on top of their deck
        (for {
          (card, g2) <- g.pick(_ == Silver)
          _ = self.Logger.info(s"${self.name} gains 1 Silver and puts it on top of his deck")
          g3 = g2.update(self.deckLens.modify(card +: _))
          victims = g3.victims(self)
        } yield {
          victims.foldLeft(g3) { (state, v) =>
            v.hand.pick(c => c == Estate || c == Duchy || c == Province) match {
              case Some((c, newHand)) =>
                self.Logger.info(s"${self.name} reveals ${c.name} and puts it on top of his deck")
                state.update(v.deckLens.modify(c +: _))
              case _ =>
                self.Logger.info(s"${self.name} does not have any Victory card and reveals his hand")
                state
            }
          }
        }).getOrElse(g)
      case Cellar =>
        // Discard N cards, draw N cards, +1 action
        val (discarded, newHand) = self.hand.partition(c => self.strategy.discardForCellar(self.hand).contains(c))
        self.Logger.info(s"${self.name} discards $discarded")
        val p2 = self.handLens.set(newHand).discardedLens.modify(discarded ++ _)

        g.update(p2.drawsN(discarded.size).gainsActions(1))
      case Chapel =>
        // Trash up to 4 cards from the hand
        (for {
          (picked, remaining) <- self.strategy.pickCardsToTrash(self.hand)
          g2 = g.update(self.handLens.set(remaining))
        } yield {
          picked.foldLeft(g2) { (state, card) =>
            self.Logger.info(s"${self.name} trashes $card")
            state.trash(card)
          }
        }).getOrElse(g)
      case CouncilRoom =>
        // Draw 4 cards, +1 buy, every other player draws 1 card
        val g2 = g.victims(self).foldLeft(g) { (state, victim) =>
          state.update(victim.draws)
        }
        g2.update(self.drawsN(4).gainsBuys(1))
      case Feast =>
        // Trash this card and gain one costing up to 5 coins
        (for {
          (feast, newDiscarded) <- self.discarded.pick(_ == Feast)
          cardForFeast = self.strategy.selectCardForFeast(g.availableCards)
          (card, g2) <- g.pick(_ == cardForFeast)
        } yield {
          self.Logger.info(s"${self.name} trashes Feast")
          self.Logger.info(s"${self.name} gains ${card.name}")
          g2.trash(feast).update(self.discardedLens.set(card +: newDiscarded))
        }).getOrElse(g)
      case Festival =>
        // Draw +2 actions, +1 buy, +2 coins
        g.update(self.gains(Turn(2, 1, Coins(2))))
      case Laboratory =>
        // Draw 2 cards, +1 action
        g.update(self.drawsN(2).gainsActions(1))
      case Market =>
        // Draw 1 card, +1 action, +1 buy, +1 coin
        g.update(self.draws.gains(Turn(1, 1, Coins(1))))
      case Militia =>
        // Gain +2 coins, every other player discards cards until they have 3 cards in their hand
        val g2 = g.victims(self).foldLeft(g) { (gn, pn) =>
          val toDiscard = pn.strategy.discardForMilitia(pn.hand)
          gn.update(toDiscard.foldLeft(pn)((player, card) => player.discard(card)))
        }

        g2.update(self.gainsCoins(Coins(2)))
      case Mine =>
        // Trash 1 treasure card and get one whose cost is +3
        (for {
          treasure <- self.strategy.pickTreasureToTrash(self.hand)
          (_, newHand) <- self.hand.pick(_ == treasure)
          p = self.handLens.set(newHand)
          (newTreasure, g2) <- g.trash(treasure).pick(treasureByCost(treasure.cost + Coins(3)))
        } yield {
          self.Logger.info(s"${self.name} trashes ${treasure.name} and gains ${newTreasure.name}")
          g2.update(p.handLens.modify(newTreasure +: _))
        }).getOrElse(g)
      case Moat =>
        // Draw 2 cards
        g.update(self.drawsN(2))
      case Moneylender =>
        // Trash a Copper and gain +3 coins
        self.hand.pick(_ == Copper).fold(g) {
          case (copper, newHand) =>
            self.Logger.info(s"${self.name} trashes a Copper")
            g.trash(copper).update(self.handLens.set(newHand).gainsCoins(Coins(3)))
        }
      case Remodel =>
        // Trash a card and gain one that costs up to 2 coins more
        (for {
          (cardToTrash, newHand) <- self.strategy.pickCardToTrash(self.hand)
          _ = self.Logger.debug(s"${self.name} wants to trash ${cardToTrash.name}")
          cardToGain <- self.strategy.pickCardToGain(g.availableCards)(cardToTrash.cost)
          _ = self.Logger.debug(s"${self.name} wants to gain ${cardToGain.name}")
          (card, g2) <- g.pick(_ == cardToGain)
          p = self.handLens.set(newHand).discardedLens.modify(card +: _)
        } yield {
          self.Logger.info(s"${self.name} trashes ${cardToTrash.name} and gains ${card.name}")
          g2.trash(cardToTrash).update(p)
        }).getOrElse(g)
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
      case ThroneRoom =>
        // Choose an action in your hand and play it twice
        self.strategy.selectActionForThroneRoom(self.hand).fold(g) { a =>
          val g2 = g.update(self.discard(a))
          self.Logger.info(s"${self.name} decides to play twice ${a.name}")
          Seq(a, a).foldLeft(g2) { (gn, _) =>
            gn.find(self).playAction(a)(gn)
          }
        }
      case Village =>
        // Draw +1 card, +2 actions
        g.update(self.draws.gainsActions(2))
      case Witch =>
        // Draw 2 cards, give one curse to all other players
        val g2 = g.update(self.drawsN(2))

        g2.victims(self).foldLeft(g2) { (gn, pn) =>
          if (gn.curses > 0)
            gn.drawCurse.update(pn.deckLens.modify(Curse +: _))
          else gn
        }
      case Woodcutter =>
        // Gain +1 buy, +2 coins
        g.update(self.gains(Turn(0, 1, Coins(2))))
      case Workshop =>
        // Gain a card costing up to 4 coins
        val cardForWorkshop = self.strategy.selectCardForWorkshop(g.availableCards)
        g.pick(_ == cardForWorkshop).fold(g) {
          case (card, g2) =>
            self.Logger.info(s"${self.name} gains ${card.name}")
            g2.update(self.discardedLens.modify(card +: _))
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

