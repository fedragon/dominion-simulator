package com.github.fedragon.dominion

import TreasureCards.{Copper, Gold, Silver}
import VictoryCards._

import scalaz.Scalaz._

class DominionSpec extends UnitSpec {

  "Dominion" should "let players play a game" in {
    Dominion.playGame(Vector("X", "Y"))(Some(1))
  }

  it should "create a player with a starting deck" in {
    val subject: Player = Dominion.createPlayer("P")

    val expectedCards = Deck.fillWith(7)(Copper) ++ Deck.fillWith(3)(Estate)

    subject.name shouldBe "P"
    subject.hand.size shouldBe 5
    subject.deck.size shouldBe 5
    subject.discarded shouldBe 'empty
    (subject.deck ++ subject.hand) should contain theSameElementsAs expectedCards
  }

  it should "create a random set of 10 Kingdom Cards having 10 cards each" in {
    val set: Map[Card, Int] = Dominion.createKingdomSet

    if (set.contains(Gardens)) {
      set(Gardens) shouldBe 12
      set.filterKeys(_ =/= Gardens).forall(_._2 == 10) shouldBe true
    } else {
      set.forall(_._2 == 10) shouldBe true
    }
  }

  it should "create the starting deck for a game with 2 players" in {
    val subject = Dominion.createStartingDeck(2)

    val kingdoms = subject.filter {
      case (_: Kingdom, _) => true
      case _ => false
    }

    val treasures = subject.filter {
      case (_: Treasure, _) => true
      case _ => false
    }

    val victories = subject.filter {
      case (Gardens, _) => false
      case (_: Victory, _) => true
      case _ => false
    }

    subject(Copper) shouldBe 46
    subject(Silver) shouldBe 40
    subject(Gold) shouldBe 30
    subject(Curse) shouldBe 10
    subject(Estate) shouldBe 8
    subject(Duchy) shouldBe 8
    subject(Province) shouldBe 8

    kingdoms.size shouldBe 10
    if (kingdoms.contains(Gardens)) {
      kingdoms(Gardens) shouldBe 12
      kingdoms.filterKeys(_ =/= Gardens).forall(_._2 == 10) shouldBe true
    } else {
      kingdoms.forall(_._2 == 10) shouldBe true
    }

    treasures.keys should contain theSameElementsAs Deck(Copper, Silver, Gold)
    victories.keys should contain theSameElementsAs Deck(Curse, Estate, Duchy, Province)
  }
}

