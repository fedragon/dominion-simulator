package com.github.fedragon.dominion

import Deck._
import KingdomCards._
import TreasureCards._
import VictoryCards._
import org.slf4j.LoggerFactory

import util.Random

object Dominion {

  val Logger = LoggerFactory.getLogger(getClass)

  private val KingdomCards: Seq[Card] = Seq(
    Bureaucrat,
    Cellar,
    Chapel,
    CouncilRoom,
    Feast,
    Festival,
    Laboratory,
    Market,
    Militia,
    Mine,
    Moat,
    Moneylender,
    Remodel,
    Smithy,
    Spy,
    Thief,
    ThroneRoom,
    Village,
    Witch,
    Woodcutter,
    Workshop
  )

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

  def playGame(playerNames: Vector[String]): Unit = {
    val players = playerNames.map(createPlayer).toMap

    val supplyPiles = createStartingDeck(players.size)

    Logger.info(s"Starting set of cards: $supplyPiles")

    var game = Game(players, supplyPiles, EmptyDeck)

    while (!game.ended) {
      game = players.values.foldLeft(game) { (g, player) =>
        player.playTurn(g)
      }

      Logger.info("All players played their turn.")
      Logger.info(s"Game state: $game")
    }

    game.ranking.foreach(println)
  }

  private def createPlayer(name: String) =
    name -> Player(name, deck = Deck.fillWith(7)(Copper) ++ Deck.fillWith(3)(Estate))

  private def createStartingDeck(nOfPlayers: Int): Map[Card, Int] =
    Random.shuffle(KingdomCards).take(10).map(_ -> 10).toMap ++ TreasureCards(nOfPlayers) ++ VictoryCards

}
