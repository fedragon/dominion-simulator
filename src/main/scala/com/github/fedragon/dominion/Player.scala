package com.github.fedragon.dominion

import Deck._

sealed trait Strategy {
  def whatToDiscard(cards: Deck): Deck
}

case object DefaultStrategy extends Strategy {
  // TODO improve
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

  def discard(card: Card): Player = copy(hand = hand.pick(card), discarded = card +: discarded)
 
  def discardHand: Player = copy(hand = EmptyDeck, discarded = discarded ++ hand)

  def draws: Player =
    deck.draw match {
      case Some((card, newDeck)) => copy(hand = card +: hand, deck = newDeck)
      case None =>
        val (card, newDeck) = (deck ++ discarded).shuffle.draw.get
        copy(hand = card +: hand, discarded = EmptyDeck, deck = newDeck)
    }

  def drawsN(n: Int): Player = (0 until n).foldLeft(this)((p, _) => p.draws)

  def plays(a: Action): Player = {
    validateAction(a).fold(this) { _ =>
      // discard this action, play it and update the turn
      val afterAction = playAction(a)(discard(a))
      afterAction.copy(turn = afterAction.turn.decrActions(1))
    }
  }

  def withBonus(t: Turn): Player = copy(turn = turn + t)

  private def validateAction(a: Action) =
    if (turn.hasActions) hand.find(_ == a)
    else None
}

object Player {
  def apply(name: String, hand: Deck, deck: Deck) =
    new Player(name = name, hand = hand, deck = deck)

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
