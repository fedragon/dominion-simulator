package com.github.fedragon.dominion

import Deck._
import KingdomCards.{Moat, Smithy, Spy}
import TreasureCards.{Copper, Silver}
import VictoryCards.{Curse, Duchy, Province}

class DeckSpec extends UnitSpec {

  val subject = Deck(Copper, Duchy, Moat, Province, Silver, Smithy)

  "Drawing from a deck" should "reduce the number of remaining cards" in {
    subject.size shouldBe 6
    val (_, newDeck) = subject.draw.get

    newDeck.size shouldBe 5
  }

  "A deck" should "be able to return all the treasure cards in it" in {
    subject.onlyTreasures should contain theSameElementsAs Deck(Copper, Silver)
  }

  it should "be able to return all the victory cards in it" in {
    subject.onlyVictories should contain theSameElementsAs Deck(Duchy, Province)
  }

  "Picking from a deck" should "return the card, if available" in {
    subject.pick(_ == Province) match {
      case Some((card, newDeck)) =>
        card shouldBe Province
        newDeck should contain theSameElementsAs Deck(Copper, Duchy, Moat, Silver, Smithy)
      case _ => fail()
    }
  }

  it should "return a number of cards, if available" in {
    val deck = Deck(Curse, Curse, Province, Silver, Smithy)

    deck.pickN(4)(_ == Curse) match {
      case Some((cards, newDeck)) =>
        cards should contain theSameElementsAs Deck(Curse, Curse)
        newDeck should contain theSameElementsAs Deck(Province, Silver, Smithy)
      case _ => fail()
    }
  }

  it should "not return the card, if not available" in {
    subject.pick(_ == Spy) shouldBe None
  }
}

