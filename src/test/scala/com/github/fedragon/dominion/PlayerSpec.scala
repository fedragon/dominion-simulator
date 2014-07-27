package com.github.fedragon.dominion

import Deck._
import TreasureCards._
import VictoryCards._
import KingdomCards._

class PlayerSpec extends UnitSpec {

  "A player" should "be able to draw cards from his deck" in {
    val pStateOne = new Player("P", deck = Deck(Copper, Estate)).draws
    pStateOne.hand should contain(Copper)
    pStateOne.deck should contain only Estate

    val pStateTwo = pStateOne.draws
    pStateTwo.deck shouldBe 'empty
    pStateTwo.hand should contain only(Copper, Estate)
  }

  it should "be able to discard his hand" in {
    val pStateOne = new Player("P", deck = Deck(Copper, Estate)).draws
    pStateOne.hand should contain(Copper)

    val pStateTwo = pStateOne.discardHand
    pStateTwo.hand shouldBe 'empty
    pStateTwo.discarded should contain(Copper)
  }

  it should "play an action, if there is at least one such action in his hand" in {
    val subject = Player("P", hand = Deck(Smithy), deck = Deck(Copper, Estate, Copper))

    val (player, _) = subject.plays(Smithy)(Game(Map(subject.name -> subject), EmptyDeck, EmptyDeck))

    player.hand.size shouldBe 3
    player.deck shouldBe 'empty
  }

  it should "not play an action, if it is not in his hand" in {
    val subject = new Player("P", deck = Deck(Copper, Estate))

    val (player, _) = subject.plays(Smithy)(Game(Map(subject.name -> subject), EmptyDeck, EmptyDeck))

    player shouldBe subject
  }

  it should "not be allowed to play more actions than he can" in {
    val subject = Player("P", hand = Deck(Smithy), deck = Deck(Copper, Estate, Copper))
    val game = Game(Map(subject.name -> subject), EmptyDeck, EmptyDeck)

    // Smithy does not affect `game` so there's no problem reusing the same instance
    val (pStateOne, _) = subject.plays(Smithy)(game)
    val (pStateTwo, _) = pStateOne.plays(Smithy)(game)

    pStateOne should not equal subject
    pStateTwo shouldEqual pStateOne
  }

  it should "not be able to buy a card, if he does not have enough coins" in {
    val subject = Player("P", hand = Deck(Copper), deck = EmptyDeck)
    val game = Game(Map(subject.name -> subject), Deck(Moat), EmptyDeck)

    val (pStateOne, gStateOne) = subject.buys(Moat)(game)

    pStateOne.buysLens.get shouldBe 1
    pStateOne.hand should contain only Copper
    gStateOne.cards should contain only Moat
  }

  it should "be able to buy a card, if he has enough coins in his hand" in {
    val subject = Player("P", hand = Deck(Silver), deck = EmptyDeck)
    val game = Game(Map(subject.name -> subject), Deck(Moat), EmptyDeck)

    val (pStateOne, gStateOne) = subject.buys(Moat)(game)

    pStateOne.buysLens.get shouldBe 0
    pStateOne.hand should contain only Moat
    gStateOne.cards shouldBe 'empty
  }

  it should "be able to buy a card, if he has enough coins including extra coins" in {
    val subject = Player("P", hand = Deck(Copper), deck = EmptyDeck, turn = Turn(0, 1, Coins(1)))
    val game = Game(Map(subject.name -> subject), Deck(Moat), EmptyDeck)

    val (pStateOne, gStateOne) = subject.buys(Moat)(game)

    pStateOne.buysLens.get shouldBe 0
    pStateOne.hand should contain only Moat
    gStateOne.cards shouldBe 'empty
  }

  // TODO test buys when the preferred card is not available in the main deck
}
