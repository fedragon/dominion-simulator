package com.github.fedragon.dominion

sealed trait Strategy {
  def whatToDiscard(cards: Cards): Cards
}

case object DefaultStrategy extends Strategy {
  def whatToDiscard(cards: Cards): Cards = cards.headOption.toVector
}

case class Player(name: String,
                  strategy: Strategy = DefaultStrategy,
                  hand: Cards = ZeroCards,
                  discarded: Cards = ZeroCards,
                  deck: Deck,
                  turn: Turn = Turn(actions = 1, buys = 1, coins = 0)) extends PlayerOps {

  import Player._

  def canBuy(that: Card): Boolean = {
    val coins = hand.count { case (_: Treasure) => true; case _ => false}
    coins >= that.cost.value
  }

  // TODO rename to draws
  def draw: Player =
    deck.draw match {
      case Some((card, newDeck)) => copy(hand = card +: hand, deck = newDeck)
      case None =>
        val (card, newDeck) = deck.insert(discarded).shuffle.draw.get
        copy(hand = card +: hand, discarded = ZeroCards, deck = newDeck)
    }

  def drawN(n: Int): Player = {
    //    def drawN(i: Int, p: Player): Player =
    //      if (i == 0) p
    //      else drawN(i - 1, p.draw)
    //
    //    drawN(n, this)

    (0 until n).foldLeft(this)((p, _) => p.draw)
  }

  def discardWithStrategy: Player = {
    val (discarded, newHand) = hand.partition(c => strategy.whatToDiscard(hand).contains(c))
    copy(hand = newHand, discarded = this.discarded ++ discarded)
  }

  def discardHand: Player = copy(hand = ZeroCards, discarded = discarded ++ hand)

  def withBonus(t: Turn): Player = copy(turn = turn + t)

  def discard(card: Card): Player = {
    val index = hand.indexOf(card)

    val newHand =
      if (hand.isDefinedAt(index))
        hand.patch(index, ZeroCards, 1)
      else hand

    copy(hand = newHand, discarded = card +: discarded)
  }

}

object Player {
  def apply(name: String, hand: Cards, deck: Deck) =
    new Player(name = name, hand = hand, deck = deck)

  def apply(name: String, hand: Cards, discarded: Cards, deck: Deck) =
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
