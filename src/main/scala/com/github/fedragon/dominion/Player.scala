package com.github.fedragon.dominion

case class Player(name: String,
                  hand: Cards = ZeroCards,
                  discarded: Cards = ZeroCards,
                  deck: Deck,
                  turn: Turn = Turn(actions = 1, buys = 1, coins = 0)) {

  import Player._

  def canBuy(that: Card): Boolean = {
    val coins = hand.count { case (_: Treasure) => true; case _ => false}
    coins >= that.cost.value
  }

  def draw: Player =
    deck.draw match {
      case Some((card, newDeck)) => copy(hand = card +: hand, deck = newDeck)
      case None =>
        val (card, newDeck) = deck.insert(discarded).shuffle.draw.get
        copy(hand = card +: hand, discarded = ZeroCards, deck = newDeck)
    }

  def discard(card: Card): Player = {
    val index = hand.indexOf(card)

    val newHand =
      if (hand.isDefinedAt(index))
        hand.patch(index, ZeroCards, 1)
      else hand

    copy(hand = newHand, discarded = card +: discarded)
  }

  def discardHand: Player = copy(hand = ZeroCards, discarded = discarded ++ hand)

  def plays(a: Action): Player =
    validateAction(a).fold(this) { _ =>
      (a.play _ andThen afterAction(a))(this)
    }

  def validateAction(a: Action) =
    if (turn.hasActions) hand.find(_ == a)
    else None

  def withBonus(t: Turn): Player = copy(turn = turn + t)
}

object Player {

  private def afterAction(a: Action)(p: Player): Player = {
    val p2 = p.discard(a)
    p2.copy(turn = p2.turn.decrActions(1))
  }
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
