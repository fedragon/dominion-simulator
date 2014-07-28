package com.github.fedragon.dominion

import Deck._
import scalaz.Scalaz._

sealed trait Strategy {
  def whatToDiscard(cards: Deck): Deck
}

trait DefaultStrategy extends Strategy {
  // TODO improve
  self: Player =>

  def sortByPreference(actions: Actions): Actions = actions.sortWith(_.cost > _.cost)

  def buyPreference(cards: Deck): Deck = cards.sortWith(_.cost > _.cost)

  override def whatToDiscard(cards: Deck): Deck =
    cards.draw.map {
      case (card, _) => Deck(card)
    }.getOrElse(EmptyDeck)
}

case class Player(name: String,
                  hand: Deck = EmptyDeck,
                  discarded: Deck = EmptyDeck,
                  deck: Deck,
                  turn: Turn = Turn(actions = 1, buys = 1, coins = Coins(0))) extends PlayerOps with DefaultStrategy {

  import Player._
  import monocle.syntax._

  val handLens = this |-> _hand
  val deckLens = this |-> _deck

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
        val (_, cardsToDiscard) = treasures.foldLeft((diff, EmptyDeck)) { (state, treasure) =>
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
      case (_, newHand) => copy(hand = newHand, discarded = card +: discarded)
    }
  }

  def discard(cards: Deck): Player = {
    cards.foldLeft(this) { (p, card) =>
      p.discard(card)
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

  def playRound(game: Game): Game = {

    def playActions(p: Player, g: Game): (Player, Game) = {
      val actions = sortByPreference(p.handLens.get.collect {
        case Action(a) => a
      })

      actions.foldLeft((p, g)) { (state, action) =>
        val (px, gx) = state
        if (px.actionsLens.get > 0)
          px.plays(action)(gx)
        else (px, gx)
      }
    }

    def playBuys(p: Player, g: Game): (Player, Game) = {
      // TODO should be decided by the strategy
      val preferredCards = g.cards.groupBy(_.name).map(_._2.head)

      preferredCards.foldLeft((p, g)) { (state, card) =>
        val (px, gx) = state

        if (px.buysLens.get > 0)
          px.buys(card)(gx)
        else (px, gx)
      }
    }

    // Actions Phase
    val (p1, g1) = playActions(this, game)

    // Buy Phase
    val (p2, g2) = playBuys(p1, g1)

    // Cleanup Phase: discard hand and draw next 5 cards
    g2.update(p2.discardHand.drawsN(5))
  }

  def treasures: Treasures = handLens.get.collect {
    case Treasure(t) => t
  }

  def victories: Victories = {
    def from(d: Deck) = deck.collect {
      case Victory(v) => v
    }

    from(hand) ++ from(discarded) ++ from(deck)
  }

  private def validateAction(a: Action) =
    if (actionsLens.get > 0) hand.find(_ === a)
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

