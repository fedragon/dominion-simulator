package com.github.fedragon.dominion

import Deck._

sealed trait Strategy {
  def nextAction(cards: Deck): Option[Action]
  def whatToDiscard(cards: Deck): Deck
}

trait DefaultStrategy extends Strategy {
  // TODO improve

  override def nextAction(cards: Deck): Option[Action] =
    cards.collect {
      case Action(a) => a
    }.headOption

  override def whatToDiscard(cards: Deck): Deck =
    cards.draw.map {
      case (card, _) => Vector(card)
    }.getOrElse(EmptyDeck)
}

case class Player(name: String,
                  hand: Deck = EmptyDeck,
                  discarded: Deck = EmptyDeck,
                  deck: Deck,
                  turn: Turn = Turn(actions = 1, buys = 1, coins = 0)) extends PlayerOps with DefaultStrategy {

  import Player._
  import monocle.syntax._

  val handLens = this |-> _hand
  val discardedLens = this |-> _discarded
  val deckLens = this |-> _deck

  val turnLens = this |-> _turn
  val actionsLens = turnLens |-> _actions
  val coinsLens = turnLens |-> _coins

  def canBuy(that: Card): Boolean = {
    val coins = hand.count { case (_: Treasure) => true; case _ => false}
    coins >= that.cost.value
  }

  def discard(card: Card): Player = {
    hand.pick(_ == card).fold(this) {
      case (_, newHand) => copy(hand = newHand, discarded = card +: discarded)
    }
  }

  def discardHand: Player = copy(hand = EmptyDeck, discarded = discarded ++ hand)

  def draws: Player =
    deck.draw match {
      case Some((card, newDeck)) =>
        copy(hand = card +: hand, deck = newDeck)
      case None =>
        val (card, newDeck) = (deck ++ discarded).shuffle.draw.get
        copy(hand = card +: hand, discarded = EmptyDeck, deck = newDeck)
    }

  def drawsN(n: Int): Player = (0 until n).foldLeft(this)((p, _) => p.draws)

  def plays(a: Action)(g: Game): (Player, Game) = {
    validateAction(a).fold((this, g)) { _ =>
      // discard this action and update the turn, then play the action
      withPlayer(discard(a)) { p =>
        withPlayer(p.actionsLens.modify(_ - 1)) { p2 =>
          val game = playAction(a)(p2)(g)
          (game.find(this), game)
        }
      }
    }
  }

  private def validateAction(a: Action) =
    if (actionsLens.get > 0) hand.find(_ == a)
    else None
}

object Player {

  import monocle.Macro._

  val _hand = mkLens[Player, Deck]("hand")
  val _discarded = mkLens[Player, Deck]("discarded")
  val _deck = mkLens[Player, Deck]("deck")

  val _turn = mkLens[Player, Turn]("turn")
  val _actions = mkLens[Turn, Int]("actions")
  val _buys = mkLens[Turn, Int]("buys")
  val _coins = mkLens[Turn, Int]("coins")

  def apply(name: String, hand: Deck, discarded: Deck, deck: Deck) =
    new Player(name = name, hand = hand, discarded = discarded, deck = deck)
}

case class Turn(actions: Int, buys: Int, coins: Int) {
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

