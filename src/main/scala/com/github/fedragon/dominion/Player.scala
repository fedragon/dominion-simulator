package com.github.fedragon.dominion

import Deck._

sealed trait Strategy {
  def nextAction(cards: Deck): Option[Action]
  def whatToDiscard(cards: Deck): Deck
}

case object DefaultStrategy extends Strategy {
  // TODO improve

  def nextAction(cards: Deck): Option[Action] = cards.cards.collect { case Action(a) => a}.headOption

  def whatToDiscard(cards: Deck): Deck = cards.draw.map(c => Deck(Vector(c._1))).getOrElse(EmptyDeck)
}

case class Player(name: String,
                  strategy: Strategy = DefaultStrategy,
                  hand: Deck = EmptyDeck,
                  discarded: Deck = EmptyDeck,
                  deck: Deck,
                  turn: Turn = Turn(actions = 1, buys = 1, coins = 0)) extends PlayerOps {

  def canBuy(that: Card): Boolean = {
    val coins = hand.count { case (_: Treasure) => true; case _ => false}
    coins >= that.cost.value
  }

  def discard(card: Card): Player = {
    hand.pick(card).fold(this) {
      case (_, newHand) => copy(hand = newHand, discarded = card +: discarded)
    }
  }

  def discardHand: Player = copy(hand = EmptyDeck, discarded = discarded ++ hand)

  def draws: Player =
    deck.draw match {
      case Some((card, newDeck)) => copy(hand = card +: hand, deck = newDeck)
      case None =>
        val (card, newDeck) = (deck ++ discarded).shuffle.draw.get
        copy(hand = card +: hand, discarded = EmptyDeck, deck = newDeck)
    }

  def drawsN(n: Int): Player = (0 until n).foldLeft(this)((p, _) => p.draws)


  def plays(a: Action)(g: Game): (Player, Game) = {
    validateAction(a).fold((this, g)) { _ =>
      // discard this action and update the turn, then play the action
      val p = withPlayer(discard(a)) {
        p => p.copy(turn = p.turn.decrActions(1))
      }

      val game = playAction(a)(p)(g)
      (game.find(this), game)
    }
  }

  def withBonus(t: Turn): Player = copy(turn = turn + t)

  private def validateAction(a: Action) =
    if (turn.hasActions) hand.find(_ == a)
    else None
}

object Player {
  def apply(name: String, hand: Deck, discarded: Deck, deck: Deck) =
    new Player(name = name, hand = hand, discarded = discarded, deck = deck)
}

case class Turn(actions: Int, buys: Int, coins: Int) {
  def decrActions(by: Int) = copy(actions = actions - by)
  def incrActions(by: Int) = copy(actions = actions + by)
  def hasActions = actions > 0

  def decrBuys(by: Int) = copy(buys = buys - by)
  def incrBuys(by: Int) = copy(buys = buys + by)
  def hasBuys = buys > 0

  def +(that: Turn) =
    copy(
      actions = actions + that.actions,
      buys = buys + that.buys,
      coins = coins + that.coins
    )
}
