package com.github.fedragon.dominion

import Deck._
import TreasureCards._
import VictoryCards._
import org.slf4j.LoggerFactory

import scalaz.Scalaz._

case class Game(players: Map[String, Player], supplyPiles: Map[Card, Int], trashed: Deck) {

  val Logger = LoggerFactory.getLogger(getClass)

  lazy val curses: Int = supplyPiles(Curse)

  val availableCards: Deck = supplyPiles.collect {
    case (card, count) if card =/= Curse && count > 0 => card
  }.toVector

  val ranking: Seq[(Player, Int)] = {
    players.map {
      case (name, player) =>
        val nOfCards = player.allCards.size
        val points = player.allVictories.map(_.value(nOfCards)).sum
        player -> points
    }.toSeq.sortWith(_._2 > _._2)
  }

  def drawCurse: Game = copy(supplyPiles = supplyPiles.updated(Curse, supplyPiles(Curse) - 1))

  val ended: Boolean =
    supplyPiles.get(Province).isEmpty || supplyPiles.count {
      case (_, 0) => true
      case _ => false
    } == 3

  def find(p: Player): Player = players(p.name)

  def pick(f: Card => Boolean): Option[(Card, Game)] = {
    supplyPiles.collectFirst {
      case (card, count) if f(card) && count > 0 =>
        (card, copy(supplyPiles = supplyPiles.updated(card, count - 1)))
    }
  }

  def trash(card: Card): Game = copy(trashed = card +: trashed)

  def update(p: Player): Game = copy(players = players.updated(p.name, p))

  def victims(attacker: Player): Vector[Player] = {
    players.filterNot {
      case (name, pn) =>
        name === attacker.name || pn.hand.exists {
          case _: (Action with Reaction) =>
            Logger.info(s"${attacker.name} reveals Moat and negates $name's attack")
            true
          case _ => false
        }
    }.values.toVector
  }

  override def toString = {
    val ps = players.values.mkString(",")
    val sps = supplyPiles.mkString(",")
    val tr = trashed.mkString(",")
    s"{ players: [$ps], supplyPiles: {$sps}, trashed: [$tr] }"
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

    game.ranking.foreach(println)
  }

  private def createPlayer(name: String) =
    name -> Player(name, deck = Deck.fillWith(7)(Copper) ++ Deck.fillWith(3)(Estate))

  private def createStartingDeck(nOfPlayers: Int): Map[Card, Int] = {
    // TODO add more cards
    TreasureCards(nOfPlayers) ++ VictoryCards
  }

}
