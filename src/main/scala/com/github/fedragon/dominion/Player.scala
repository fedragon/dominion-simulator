package com.github.fedragon.dominion

import Deck._
import com.github.fedragon.dominion.VictoryCards.Gardens
import org.slf4j.LoggerFactory

import scalaz.Scalaz._

case class Player(name: String,
                  hand: Deck = EmptyDeck,
                  discarded: Deck = EmptyDeck,
                  deck: Deck,
                  turn: Turn = Turn(actions = 1, buys = 1, coins = Coins(0)),
                  strategy: Strategy = new DefaultStrategy) extends PlayerOps with TurnOps {

  import Player._
  import monocle.syntax._

  val Logger = LoggerFactory.getLogger(getClass)

  val handLens = this |-> _hand
  val deckLens = this |-> _deck
  val discardedLens = this |-> _discarded

  val turnLens = this |-> _turn
  val remainingActions = turnLens |-> _actions
  val remainingBuys = turnLens |-> _buys
  val remainingExtraCoins = turnLens |-> _coins

  def buys(card: Card)(g: Game): (Player, Game) = {
    val cost = card.cost

    Logger.debug(s"$name wants to buy ${card.name}")

    if (cost > coins) {
      Logger.debug(s"$name cannot buy ${card.name} for ${cost.value} coins because he only has ${cost.value} coins")
      return (this, g)
    }

    val p: Player =
      if (remainingExtraCoins.get >= cost) {
        Logger.info(s"$name buys ${card.name}")
        consumesCoins(cost)
      } else {
        val diff = cost - remainingExtraCoins.get
        val (_, cardsToDiscard) = hand.onlyTreasures.foldLeft((diff, EmptyDeck)) { (state, treasure) =>
          val (remaining, cards) = state

          if (remaining === Coins(0)) (Coins(0), cards)
          else (remaining - treasure.value, treasure +: cards)
        }

        if (remainingExtraCoins.get > Coins(0))
          usesAllExtraCoins.discard(cardsToDiscard)
        else discard(cardsToDiscard)
      }

    val (p2, g2) = g.pick(_ === card).fold((p, g)) {
      case (_, gx) =>
        val px = p.handLens.modify(card +: _).consumesBuy
        Logger.info(s"$name buys ${card.name} for ${cost.value} coins")
        px -> gx.update(px)
    }

    (p2, g2.update(p2))
  }

  def coins: Coins =
    remainingExtraCoins.get + hand.foldLeft(Coins(0)) {
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
    Logger.debug(s"$name wants to play $a")
    validateAction(a).fold((this, g)) { _ =>
      Logger.info(s"$name plays $a")
      // discard this action and update the turn, then play the action
      withPlayer(discard(a)) { p =>
        withPlayer(p.consumesAction) { p2 =>
          val game = p2.playAction(a)(g)
          (game.find(p2), game)
        }
      }
    }
  }

  def playTurn(game: Game): Game = {

    def playActions(p: Player, g: Game): (Player, Game) = {
      val actions = p.hand.collect {
        case Action(a) => a
      }

      strategy.selectNextActions(actions).foldLeft((p, g)) { (state, action) =>
        val (px, gx) = state
        if (px.remainingActions.get > 0)
          px.plays(action)(gx)
        else (px, gx)
      }
    }

    def playBuys(p: Player, g: Game): (Player, Game) = {
      val preferredCards = p.strategy.makeGroceriesList(g.supplyPiles.keys.toVector)

      preferredCards.foldLeft((p, g)) { (state, card) =>
        val (px, gx) = state

        if (px.remainingBuys.get > 0)
          px.buys(card)(gx)
        else (px, gx)
      }
    }

    Logger.info(s"$name starts his turn")

    // Actions Phase: reset player turn state
    val (p1, g1) = playActions(gains(Turn(1, 1, Coins(0))), game)

    Logger.info(s"$name completed his action phase")

    // Buy Phase
    val (p2, g2) = playBuys(p1, g1)

    Logger.info(s"$name completed his buy phase")

    // Cleanup Phase: discard hand and draw next 5 cards, clean up turn state
    val newPlayer = g2.update(p2.discardHand.drawsN(5).turnLens.set(Turn(0, 0, Coins(0))))
    Logger.info(s"$name completed his turn")
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

  def allCards: Deck = hand ++ discarded ++ deck

  def allVictories: Victories = hand.onlyVictories ++ discarded.onlyVictories ++ deck.onlyVictories

  private def drawFromDeck: (Card, Player) =
    deck.draw match {
      case Some((card, newDeck)) =>
        (card, deckLens.set(newDeck))
      case None =>
        Logger.debug(s"$name is out of cards and shuffles his discarded pile")
        val (card, newDeck) = discarded.shuffle.draw.get
        (card, discardedLens.set(EmptyDeck).deckLens.set(newDeck))
    }

  private def validateAction(a: Action) = {
    if (remainingActions.get > 0) {
      Logger.debug(s"$name can play ${a.name}")
      hand.find(_ === a)
    } else {
      Logger.debug(s"$name cannot play ${a.name} because he is out of actions")
      None
    }
  }

  override def toString = {
    val h: String = hand.mkString(",")
    val di: String = discarded.mkString(",")
    val de: String = deck.mkString(",")
    s"""$name: { hand: [$h], discarded: [$di], deck: [$de], turn: $turn }""".stripMargin
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

  override def toString = s"{ actions: $actions, buys: $buys, extraCoins: ${coins.value} }"
}

trait TurnOps {
  this: Player =>

  def gains(t: Turn): Player = {
    Logger.info(s"$name gains ${t.actions} action(s), ${t.buys} buy(s), and ${t.coins.value} coin(s)")
    turnLens.modify(_ + t)
  }

  def gainsActions(n: Int): Player = {
    Logger.info(s"$name gains $n action(s)")
    remainingActions.modify(_ + n)
  }

  def gainsBuys(n: Int): Player = {
    Logger.info(s"$name gains $n buy(s)")
    remainingBuys.modify(_ + n)
  }

  def gainsCoins(n: Coins): Player = {
    Logger.info(s"$name gains ${n.value} coin(s)")
    remainingExtraCoins.modify(_ + n)
  }

  def consumesAction: Player = {
    Logger.debug(s"$name consumes one of his actions")
    remainingActions.modify(_ - 1)
  }

  def consumesBuy: Player = {
    Logger.debug(s"$name consumes one of his buys")
    remainingBuys.modify(_ - 1)
  }

  def consumesCoins(n: Coins): Player = {
    Logger.info(s"$name consumes $n extra coin(s)")
    remainingExtraCoins.modify(_ - n)
  }

  def usesAllExtraCoins: Player = {
    Logger.info(s"$name uses all his extra coin(s)")
    remainingExtraCoins.set(Coins(0))
  }
}
