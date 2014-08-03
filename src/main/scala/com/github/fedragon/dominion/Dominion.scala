package com.github.fedragon.dominion

import ActionCards._
import Deck._
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
    Gardens,
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
    Workshop)

  private def TreasureCards(nOfPlayers: Int) = Map(
    Copper -> (60 - nOfPlayers * 7),
    Silver -> 40,
    Gold -> 30)

  private def VictoryCards(nOfPlayers: Int) = Map(
    Duchy -> (if (nOfPlayers > 2) 12 else 8),
    Estate -> (if (nOfPlayers > 2) 12 else 8),
    Province -> (if (nOfPlayers > 2) 12 else 8),
    Curse -> (nOfPlayers - 1) * 10)

  def playGame(playerNames: Vector[String])(roundsLimit: Option[Int]): Unit = {
    require(playerNames.size > 1, "At least 2 players are required!")
    require(playerNames.size < 5, "Maximum 4 players allowed!")

    val players = playerNames.map(n => n -> createPlayer(n)).toMap

    val supplyPiles = createStartingDeck(players.size)

    Logger.info(s"Starting set of cards: $supplyPiles")

    var game = Game(players, supplyPiles, EmptyDeck)
    var roundsPlayed = 0

    while (!game.ended && !roundsLimit.contains(roundsPlayed)) {
      game = players.values.foldLeft(game) { (g, player) =>
        player.playTurn(g)
      }

      Logger.info(s"All players played round #$roundsPlayed.")
      Logger.info(s"Game state: $game")

      roundsPlayed = roundsPlayed + 1
    }

    Logger.info(s"Final ranking")
    game.ranking.foreach {
      case (player, score) => Logger.info(s"${player.name}: $score")
    }
  }

  private[dominion] def createKingdomSet: Map[Card, Int] =
    Random.shuffle(KingdomCards).take(10).map {
      case g: Gardens.type => g -> 12
      case other => other -> 10
    }.toMap

  private[dominion] def createPlayer(name: String): Player =
    Player(name, deck = Deck.fillWith(7)(Copper) ++ Deck.fillWith(3)(Estate))

  private[dominion] def createStartingDeck(nOfPlayers: Int): Map[Card, Int] =
    createKingdomSet ++ TreasureCards(nOfPlayers) ++ VictoryCards(nOfPlayers)
}
