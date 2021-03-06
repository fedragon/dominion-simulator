package com.github.fedragon.dominion

import ActionCards.Moat
import VictoryCards.Province

import org.slf4j.LoggerFactory
import scalaz.Scalaz._

case class Game(players: Map[String, Player], supplyPiles: Map[Card, Int], trashed: Deck, round: Int = 0) {

  val Logger = LoggerFactory.getLogger(getClass)

  val availableCards: Deck = supplyPiles.collect {
    case (card, count) if card =/= Curse && count > 0 => card
  }.toVector

  val ranking: Seq[(Player, Int)] = {
    players.map {
      case (name, player) =>
        val nOfCards = player.allCards.size
        val points = player.allVictories.map(_.value(nOfCards)).sum
        val cursesPoints = player.allCurses.map(_.value).sum
        player -> (points - cursesPoints)
    }.toSeq.sortWith(_._2 > _._2)
  }

  val ended: Boolean =
    supplyPiles.get(Province).isEmpty || supplyPiles.count {
      case (_, 0) => true
      case _ => false
    } === 3

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
          case Moat =>
            Logger.info(s"${attacker.name} reveals Moat and negates $name's attack")
            true
          case _ => false
        }
    }.values.toVector
  }

  override def toString = {
    val ps = players.values.mkString(" ", ", ", " ")
    val sps = supplyPiles.mkString(" ", ", ", " ")
    val tr = trashed.mkString(",")
    s"""{ round: $round, players: [$ps], supplyPiles: {$sps}, trashed: [$tr] }""".stripMargin
  }
}

