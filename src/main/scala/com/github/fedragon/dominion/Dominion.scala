package com.github.fedragon.dominion

import Deck._

import scalaz.Scalaz._

case class Game(players: Map[String, Player], cards: Deck, trashed: Deck) {

  import Game._
  import VictoryCards.Province
  import monocle.syntax._

  private val supplyPiles = cards.toSet

  def ended(startingSet: Set[Card]): Boolean =
    cards.find(_ === Province).isEmpty || startingSet.size - supplyPiles.size == 3

  def find(p: Player): Player = players(p.name)

  def pick(f: Card => Boolean): Option[(Card, Game)] = {
    cards.pick(f).map {
      case (card, deck) => (card, this |-> _cards set deck)
    }
  }

  def trash(card: Card): Game = this |-> _trashed modify (card +: _)

  def update(p: Player): Game = this |-> _players modify (_.updated(p.name, p))

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

object Game {

  import monocle.Macro._

  val _players = mkLens[Game, Map[String, Player]]("players")
  val _cards = mkLens[Game, Deck]("cards")
  val _trashed = mkLens[Game, Deck]("trashed")

}

object Dominion {

  import TreasureCards._
  import VictoryCards._

  def playGame(playerNames: Vector[String]) = {
    val players = playerNames.map(createPlayer).toMap

    val (deck, startingSet): (Deck, Set[Card]) = createStartingDeck(players.size)
    var game = Game(players, deck, EmptyDeck)

    while (!game.ended(startingSet)) {
      game = players.values.foldLeft(game) { (g, player) =>
        player.playTurn(g)
      }
    }

    declareWinner(game)
  }

  private def createPlayer(name: String) =
    name -> Player(name, deck = Deck.fillWith(7)(Copper) ++ Deck.fillWith(3)(Estate))

  private def createStartingDeck(nOfPlayers: Int): (Deck, Set[Card]) = {
    // TODO add more cards
    val deck = Deck.fillWith(60 - nOfPlayers * 7)(Copper) ++
      Deck.fillWith(12)(Estate) ++
      Deck.fillWith(12)(Duchy) ++
      Deck.fillWith(12)(Province)

    (deck, deck.toSet)
  }

  private def declareWinner(game: Game): Unit = {
    val ranking = game.players.map {
      case (name, player) =>
        name -> player.victories.foldLeft(CardValue(0))(_ + _.value)
    }.toSeq.sortWith(_._2 > _._2)

    ranking.foreach(println)
  }
}
