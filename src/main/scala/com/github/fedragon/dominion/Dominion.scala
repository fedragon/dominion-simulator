package com.github.fedragon.dominion

import Deck._

case class Game(players: Map[String, Player], cards: Deck, trashed: Deck) {

  import Game._
  import monocle.syntax._

  def find(p: Player) = players(p.name)

  def playersExcept(p: Player): Vector[Player] =
    players.filterNot { case (n, _) => n == p.name}.values.toVector

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
        name == p.name || pn.hand.exists {
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

