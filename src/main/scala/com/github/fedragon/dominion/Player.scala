package com.github.fedragon.dominion

import Deck._
import org.slf4j.LoggerFactory

import scalaz.Scalaz._

case class Player(name: String,
                  hand: Deck = EmptyDeck,
                  discarded: Deck = EmptyDeck,
                  deck: Deck,
                  turn: Turn = Turn(actions = 1, buys = 1, coins = Coins(0)),
                  strategy: Strategy = new DefaultStrategy) extends PlayerOps {

  import Player._
  import monocle.syntax._

  val Logger = LoggerFactory.getLogger(getClass)

  val handLens = this |-> _hand
  val deckLens = this |-> _deck
  val discardedLens = this |-> _discarded

  val turnLens = this |-> _turn
  val actionsLens = turnLens |-> _actions
  val buysLens = turnLens |-> _buys
  val extraCoinsLens = turnLens |-> _coins

  def buys(card: Card)(g: Game): (Player, Game) = {
    val cost = card.cost

    if (cost > coins) return (this, g)

    val p: Player =
      if (extraCoinsLens.get >= cost)
        extraCoinsLens.modify(_ - cost)
      else {
        val diff = cost - extraCoinsLens.get
        val (_, cardsToDiscard) = hand.onlyTreasures.foldLeft((diff, EmptyDeck)) { (state, treasure) =>
          val (remaining, cards) = state

          if (remaining === Coins(0)) (Coins(0), cards)
          else (remaining - treasure.value, treasure +: cards)
        }

        discard(cardsToDiscard)
      }

    val (p2, g2) = g.pick(_ === card).fold((p, g)) {
      case (_, gx) =>
        val px = p.handLens.modify(card +: _).buysLens.modify(_ - 1)
        px -> gx.update(px)
    }

    (p2, g2.update(p2))
  }

  def coins: Coins =
    extraCoinsLens.get + handLens.get.foldLeft(Coins(0)) {
      (acc, card) => acc + (card match {
        case t: Treasure => t.value
        case _ => Coins(0)
      })
    }

  def discard(card: Card): Player = {
    hand.pick(_ === card).fold(this) {
      case (_, newHand) =>
        Logger.info(s"$name discards ${card.name} from his hand")
        handLens.set(newHand).discardedLens.modify(card +: _)
    }
  }

  def discard(cards: Deck): Player = {
    cards.foldLeft(this) { (p, card) =>
      p.discard(card)
    }
  }

  def discardHand: Player = {
    Logger.info(s"$name discards all cards from his hand")
    handLens.set(EmptyDeck).discardedLens.modify(_ ++ hand)
  }

  def draws: Player = {
    val (card, state) = drawFromDeck
    Logger.info(s"$name draws ${card.name} from his deck to his hand")
    state.handLens.modify(card +: _)
  }

  def drawsN(n: Int): Player = (0 until n).foldLeft(this)((p, _) => p.draws)

  def plays(a: Action)(g: Game): (Player, Game) = {
    Logger.info(s"$name wants to play $a")
    val result = validateAction(a).fold((this, g)) { _ =>
      Logger.info(s"$name plays $a")
      // discard this action and update the turn, then play the action
      withPlayer(discard(a)) { p =>
        withPlayer(p.actionsLens.modify(_ - 1)) { p2 =>
          val game = p2.playAction(a)(g)
          (game.find(p2), game)
        }
      }
    }
    Logger.info(s"$name played ${a.name}")
    result
  }

  def playTurn(game: Game): Game = {

    def playActions(p: Player, g: Game): (Player, Game) = {
      // TODO should be sorted according to the strategy
      val actions = p.handLens.get.collect {
        case Action(a) => a
      }

      actions.foldLeft((p, g)) { (state, action) =>
        val (px, gx) = state
        if (px.actionsLens.get > 0)
          px.plays(action)(gx)
        else (px, gx)
      }
    }

    def playBuys(p: Player, g: Game): (Player, Game) = {
      // TODO should be decided by the strategy
      val preferredCards = g.supplyPiles.keys

      preferredCards.foldLeft((p, g)) { (state, card) =>
        val (px, gx) = state

        if (px.buysLens.get > 0)
          px.buys(card)(gx)
        else (px, gx)
      }
    }

    // Actions Phase
    val (p1, g1) = playActions(this, game)

    Logger.info(s"$name completed his action phase")

    // Buy Phase
    val (p2, g2) = playBuys(p1, g1)

    Logger.info(s"$name completed his buy phase")

    // Cleanup Phase: discard hand and draw next 5 cards
    val newPlayer = g2.update(p2.discardHand.drawsN(5))
    Logger.info(s"$name completed his turn: $newPlayer")
    newPlayer
  }

  def reveals(shouldDiscard: Card => Boolean) = {
    val (card, state) = drawFromDeck
    Logger.info(s"$name reveals ${card.name} from the top of his deck")

    if (shouldDiscard(card)) {
      Logger.info(s"$name discards ${card.name} as requested by the attacker")
      state.discardedLens.modify(card +: _)
    } else {
      Logger.info(s"$name puts ${card.name} back on the top of his deck as requested by the attacker")
      state.deckLens.modify(card +: _)
    }
  }

  def revealsN(n: Int): (Deck, Player) = {
    (0 until n).foldLeft((EmptyDeck, this)) {
      (state, _) =>
        val (stash, p) = state
        val (card, p2) = p.drawFromDeck
        Logger.info(s"$name reveals ${card.name} from the top of his deck")
        (card +: stash, p2)
    }
  }

  def allVictories: Victories =
    hand.onlyVictories ++ discarded.onlyVictories ++ deck.onlyVictories

  private def drawFromDeck: (Card, Player) =
    deck.draw match {
      case Some((card, newDeck)) =>
        (card, deckLens.set(newDeck))
      case None =>
        val (card, newDeck) = (deck ++ discarded).shuffle.draw.get
        (card, discardedLens.set(EmptyDeck).deckLens.set(newDeck))
    }

  private def validateAction(a: Action) = {
    if (actionsLens.get > 0) {
      Logger.info(s"$name can play ${a.name}")
      hand.find(_ === a)
    } else {
      Logger.info(s"$name cannot play ${a.name} because he is out of actions")
      None
    }
  }
}

object Player {

  import monocle.Macro._

  val _hand = mkLens[Player, Deck]("hand")
  val _discarded = mkLens[Player, Deck]("discarded")
  val _deck = mkLens[Player, Deck]("deck")

  val _turn = mkLens[Player, Turn]("turn")
  val _actions = mkLens[Turn, Int]("actions")
  val _buys = mkLens[Turn, Int]("buys")
  val _coins = mkLens[Turn, Coins]("coins")

  def apply(name: String, hand: Deck, discarded: Deck, deck: Deck) =
    new Player(name = name, hand = hand, discarded = discarded, deck = deck)
}

case class Turn(actions: Int, buys: Int, coins: Coins) {
  def +(that: Turn) =
    copy(
      actions = actions + that.actions,
      buys = buys + that.buys,
      coins = coins + that.coins
    )
}

