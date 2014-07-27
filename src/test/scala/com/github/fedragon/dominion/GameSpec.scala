package com.github.fedragon.dominion

import Deck._
import KingdomCards.{Moat, Witch}

class GameSpec extends UnitSpec {

  "A game" should "be finished when all Provinces are gone" in {
    val subject = Game(Map.empty[String, Player], EmptyDeck, EmptyDeck)

    subject.finished shouldBe true
  }

  it should "find a player" in {
    val p1 = Player("P1", hand = Deck(Witch), deck = EmptyDeck)
    val subject = Game(Map(p1.name -> p1), EmptyDeck, EmptyDeck)

    subject.find(p1) shouldBe p1
  }

  it should "pick a card, if still available" in {
    val subject = Game(Map.empty[String, Player], Deck(Moat), EmptyDeck)

    val result = subject.pick(_ === Moat)
    result shouldBe 'defined

    val (card, game) = result.get
    card shouldBe Moat
    game.cards shouldBe 'empty

    subject.pick(_ === Witch) shouldBe None
  }

  it should "trash a card" in {
    val subject = Game(Map.empty[String, Player], EmptyDeck, EmptyDeck)

    subject.trash(Moat).trashed should contain only Moat
  }

  it should "update a player" in {
    val p1 = Player("P1", hand = Deck(Witch), deck = EmptyDeck)
    val subject = Game(Map(p1.name -> p1), EmptyDeck, EmptyDeck)

    val p2 = p1.discard(Witch)

    subject.update(p2).find(p2) shouldBe p2
  }

  it should "find all the potential victims of an attack" in {
    val p1 = Player("P1", hand = Deck(Witch), deck = EmptyDeck)
    val p2 = Player("P2", hand = Deck(Moat), deck = EmptyDeck)
    val p3 = Player("P3", hand = EmptyDeck, deck = EmptyDeck)

    val subject = Game(Map(p1.name -> p1, p2.name -> p2, p3.name -> p3), EmptyDeck, EmptyDeck)

    subject.victims(p1) should contain only p3
  }

}

