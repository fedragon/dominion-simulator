package com.github.fedragon.dominion

import Deck._
import KingdomCards.{Market, Moat, Witch}
import TreasureCards.Copper
import VictoryCards.{Gardens, Duchy, Estate, Province}

class GameSpec extends UnitSpec {

  "A game" should "end when all Provinces are gone" in {
    val subject = Game(Map.empty[String, Player], Map(Copper -> 1, Moat -> 1), EmptyDeck)

    subject.ended shouldBe true
  }

  it should "end when any 3 supply piles are empty" in {
    val subject = Game(Map.empty[String, Player], Map(Copper -> 1, Moat -> 1, Market -> 1, Province -> 1), EmptyDeck)

    subject.ended shouldBe false

    val stateOne = subject.copy(supplyPiles = Map(Copper -> 1, Moat -> 1))

    stateOne.ended shouldBe true
  }

  it should "calculate the final ranking" in {
    val p1 = Player("P1", hand = Deck(Estate), deck = Deck(Province))
    val p2 = Player("P2", hand = Deck(Duchy), deck = EmptyDeck)
    val p3 = Player("P3", hand = Deck(Gardens, Province), deck = Deck.fillWith(20)(Copper))

    val subject = Game(Map(p1.name -> p1, p2.name -> p2, p3.name -> p3), Map.empty, EmptyDeck)
    val ranking = subject.calculateRanking.map {
      case (player, points) => player.name -> points
    }

    ranking shouldBe Seq("P3" -> 8, "P1" -> 7, "P2" -> 3)
  }

  it should "find a player" in {
    val p1 = Player("P1", hand = Deck(Witch), deck = EmptyDeck)
    val subject = Game(Map(p1.name -> p1), Map.empty, EmptyDeck)

    subject.find(p1) shouldBe p1
  }

  it should "allow a player to pick a card from a supply pile, if still available" in {
    val subject = Game(Map.empty[String, Player], Map(Moat -> 1), EmptyDeck)

    val result = subject.pick(_ === Moat)
    result shouldBe 'defined

    val (card, game) = result.get
    card shouldBe Moat
    game.supplyPiles.loneElement shouldBe (Moat -> 0)

    subject.pick(_ === Witch) shouldBe None
  }

  it should "not allow a player to pick a card from a supply pile, if no longer available" in {
    val subject = Game(Map.empty[String, Player], Map(Moat -> 0), EmptyDeck)

    subject.pick(_ === Moat) shouldBe None

    subject.pick(_ === Witch) shouldBe None
  }

  it should "trash a card" in {
    val subject = Game(Map.empty[String, Player], Map.empty, EmptyDeck)

    subject.trash(Moat).trashed.loneElement shouldBe Moat
  }

  it should "update the state of a player" in {
    val p1 = Player("P1", hand = Deck(Witch), deck = EmptyDeck)
    val subject = Game(Map(p1.name -> p1), Map.empty, EmptyDeck)

    val p2 = p1.discard(Witch)

    subject.update(p2).find(p2) shouldBe p2
  }

  it should "find all the potential victims of an attack" in {
    val p1 = Player("P1", hand = Deck(Witch), deck = EmptyDeck)
    val p2 = Player("P2", hand = Deck(Moat), deck = EmptyDeck)
    val p3 = Player("P3", hand = EmptyDeck, deck = EmptyDeck)

    val subject = Game(Map(p1.name -> p1, p2.name -> p2, p3.name -> p3), Map.empty, EmptyDeck)

    subject.victims(p1).loneElement shouldBe p3
  }

}

