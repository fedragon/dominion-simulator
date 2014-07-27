package com.github.fedragon.dominion

import Deck._
import TreasureCards._
import VictoryCards._
import KingdomCards._

class PlayerSpec extends UnitSpec {

  "A player" should "be able to draw cards from his deck" in {
    val stateOne = new Player("P", deck = Deck(Copper, Estate)).draws
    stateOne.hand should contain(Copper)
    stateOne.deck should contain only Estate

    val stateTwo = stateOne.draws
    stateTwo.deck shouldBe 'empty
    stateTwo.hand should contain only(Copper, Estate)
  }

  it should "be able to discard his hand" in {
    val stateOne = new Player("P", deck = Deck(Copper, Estate)).draws
    stateOne.hand should contain(Copper)

    val stateTwo = stateOne.discardHand
    stateTwo.hand shouldBe 'empty
    stateTwo.discarded should contain(Copper)
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
    val (stateOne, _) = subject.plays(Smithy)(game)
    val (stateTwo, _) = stateOne.plays(Smithy)(game)

    stateOne should not equal subject
    stateTwo shouldEqual stateOne
  }

  it should "be able to buy a card, if he has enough coins" in {
    val subject = Player("P", hand = Deck(Silver), deck = EmptyDeck)
    val game = Game(Map(subject.name -> subject), Deck(Moat), EmptyDeck)

    val (stateOne, gameOne) = subject.buys(Moat)(game)

    stateOne.hand should contain only Moat
    gameOne.cards shouldBe 'empty
  }

  // TODO test buys with extraCoins and when the preferred card is not available in the main deck
}
