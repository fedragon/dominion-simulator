package com.github.fedragon.dominion

import Deck._

import TreasureCards._
import VictoryCards._

import scalaz.Scalaz._

case class Game(players: Map[String, Player], supplyPiles: Map[Card, Int], trashed: Deck) {

  lazy val curses: Int = supplyPiles(Curse)

  def drawCurse: Game = copy(supplyPiles = supplyPiles.updated(Curse, supplyPiles(Curse) - 1))

  val ended: Boolean =
    supplyPiles.get(Province).isEmpty || supplyPiles.count {
      case (_, 0) => true
      case _ => false
    } == 3

  def find(p: Player): Player = players(p.name)

  def pick(f: Card => Boolean): Option[(Card, Game)] = {
    supplyPiles.find {
      case (card, _) if f(card) => true
      case _ => false
    }.flatMap {
      case (card, count) =>
        Some(card, copy(supplyPiles = supplyPiles.updated(card, count - 1)))
    }
  }

  def trash(card: Card): Game = copy(trashed = card +: trashed)

  def update(p: Player): Game = copy(players = players.updated(p.name, p))

  def victims(p: Player): Vector[Player] = {
    players.filterNot {
      case (name, pn) =>
        name === p.name || pn.hand.exists {
          case _: (Action with Reaction) => true
          case _ => false
        }
    }.values.toVector
  }
}

object Dominion {

  private def TreasureCards(nOfPlayers: Int) = Map(
    Copper -> (60 - nOfPlayers * 7),
    Silver -> 40,
    Gold -> 30)

  private val VictoryCards = Map(
    Duchy -> 12,
    Estate -> 12,
    Province -> 12,
    Curse -> 30
  )

  def playGame(playerNames: Vector[String]) = {
    val players = playerNames.map(createPlayer).toMap

    val supplyPiles = createStartingDeck(players.size)
    var game = Game(players, supplyPiles, EmptyDeck)

    while (!game.ended) {
      game = players.values.foldLeft(game) { (g, player) =>
        player.playTurn(g)
      }
    }

    declareWinner(game)
  }

  private def createPlayer(name: String) =
    name -> Player(name, deck = Deck.fillWith(7)(Copper) ++ Deck.fillWith(3)(Estate))

  private def createStartingDeck(nOfPlayers: Int): Map[Card, Int] = {
    // TODO add more cards
    TreasureCards(nOfPlayers) ++ VictoryCards
  }

  private def declareWinner(game: Game): Unit = {
    val ranking = game.players.map {
      case (name, player) =>
        name -> player.allVictories.foldLeft(CardValue(0))(_ + _.value)
    }.toSeq.sortWith(_._2 > _._2)

    ranking.foreach(println)
  }
}
