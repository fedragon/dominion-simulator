package com.github.fedragon.dominion

import Deck._
import KingdomCards._
import TreasureCards._
import VictoryCards._

class PlayerSpec extends UnitSpec {

  val emptyGame = Game(Map.empty, Map.empty, EmptyDeck)

  "A player" should "be able to draw one card from his deck" in {
    val pStateOne = new Player("P", deck = Deck(Copper, Estate)).draws
    pStateOne.hand should contain(Copper)
    pStateOne.deck should contain only Estate

    val pStateTwo = pStateOne.draws
    pStateTwo.deck shouldBe 'empty
    pStateTwo.hand should contain only(Copper, Estate)
  }

  it should "be able to draw N cards from his deck" in {
    val pStateOne = new Player("P", deck = Deck(Copper, Estate)).drawsN(2)

    pStateOne.deck shouldBe 'empty
    pStateOne.hand should contain only(Copper, Estate)
  }

  it should "be able to reveal and discard the top card in his deck" in {
    val subject = new Player("P", deck = Deck(Gold, Estate))
    val pStateOne = subject.reveals(_ => false)

    pStateOne.deck should contain only(Gold, Estate)
    pStateOne.discarded shouldBe 'empty

    val pStateTwo = subject.reveals(_ => true)

    pStateTwo.deck.loneElement shouldBe Estate
    pStateTwo.discarded should contain(Gold)
  }

  it should "be able to reveal the top card in his deck even when all his cards are discarded" in {
    val subject = new Player("P", deck = EmptyDeck, discarded = Deck(Gold, Estate))
    val pStateOne = subject.reveals(_ => false)

    pStateOne.deck should contain theSameElementsAs Deck(Gold, Estate)
    pStateOne.discarded shouldBe 'empty

    val pStateTwo = subject.reveals(_ => true)

    pStateTwo.deck should contain only Estate
    pStateTwo.discarded should contain only Gold
  }

  it should "be able to know the total amount of coins he can spend" in {
    val subject = new Player("P", hand = Deck(Copper, Silver), deck = EmptyDeck, turn = Turn(1, 1, Coins(2)))

    subject.coins shouldBe Coins(5)
  }

  it should "be able to discard one card from his hand" in {
    val pStateOne = new Player("P", hand = Deck(Copper), deck = EmptyDeck)

    val pStateTwo = pStateOne.discard(Copper)

    pStateTwo.hand shouldBe 'empty
    pStateTwo.discarded should contain only Copper
  }

  it should "be able to discard N cards from his hand" in {
    val pStateOne = new Player("P", hand = Deck(Copper, Silver), deck = EmptyDeck)

    val pStateTwo = pStateOne.discard(Deck(Copper, Silver))

    pStateTwo.hand shouldBe 'empty
    pStateTwo.discarded should contain only(Copper, Silver)
  }

  it should "be able to discard his whole hand" in {
    val pStateOne = new Player("P", deck = Deck(Copper, Estate)).draws
    pStateOne.hand should contain(Copper)

    val pStateTwo = pStateOne.discardHand
    pStateTwo.hand shouldBe 'empty
    pStateTwo.discarded should contain(Copper)
  }

  it should "play an action, if there is at least one such action in his hand" in {
    val subject = Player("P", hand = Deck(Smithy), deck = Deck(Copper, Estate, Copper))
    val game = emptyGame.copy(players = Map(subject.name -> subject))

    val (player, _) = subject.plays(Smithy)(game)

    player.hand.size shouldBe 3
    player.deck shouldBe 'empty
  }

  it should "not play an action, if it is not in his hand" in {
    val subject = new Player("P", deck = Deck(Copper, Estate))
    val game = emptyGame.copy(players = Map(subject.name -> subject))

    val (player, _) = subject.plays(Smithy)(game)

    player shouldBe subject
  }

  it should "not be allowed to play more actions than he can" in {
    val subject = Player("P", hand = Deck(Smithy), deck = Deck(Copper, Estate, Copper))
    val game = emptyGame.copy(players = Map(subject.name -> subject))

    // Smithy does not affect `game` so there's no problem reusing the same instance of game
    val (pStateOne, _) = subject.plays(Smithy)(game)
    val (pStateTwo, _) = pStateOne.plays(Smithy)(game)

    pStateOne should not equal subject
    pStateTwo shouldEqual pStateOne
  }

  it should "not be able to buy a card, if he does not have enough coins" in {
    val subject = Player("P", hand = Deck(Copper), deck = EmptyDeck)
    val game = emptyGame.copy(players = Map(subject.name -> subject), supplyPiles = Map(Moat -> 1))

    val (pStateOne, gStateOne) = subject.buys(Moat)(game)

    pStateOne.buysLens.get shouldBe 1
    pStateOne.hand should contain only Copper
    gStateOne.supplyPiles should contain only (Moat -> 1)
  }

  it should "be able to buy a card, if he has enough coins in his hand" in {
    val subject = Player("P", hand = Deck(Silver), deck = EmptyDeck)
    val game = emptyGame.copy(players = Map(subject.name -> subject), supplyPiles = Map(Moat -> 1))

    val (pStateOne, gStateOne) = subject.buys(Moat)(game)

    pStateOne.buysLens.get shouldBe 0
    pStateOne.hand should contain only Moat
    gStateOne.supplyPiles should contain only (Moat -> 0)
  }

  it should "be able to buy a card, if he has enough coins including extra coins" in {
    val subject = Player("P", hand = Deck(Copper), deck = EmptyDeck, turn = Turn(0, 1, Coins(1)))
    val game = emptyGame.copy(players = Map(subject.name -> subject), supplyPiles = Map(Moat -> 1))

    val (pStateOne, gStateOne) = subject.buys(Moat)(game)

    pStateOne.buysLens.get shouldBe 0
    pStateOne.hand should contain only Moat
    gStateOne.supplyPiles should contain only (Moat -> 0)
  }

  // TODO test buys when the preferred card is not available in the main deck

  it should "be able to know all the treasure cards in his hand" in {
    val subject = new Player("P", hand = Deck(Copper, Moat, Silver, Smithy), deck = EmptyDeck)

    subject.treasures should contain only(Copper, Silver)
  }

  it should "be able to know all the victory cards in his hand, discarded pile or deck" in {
    val subject = new Player("P", hand = Deck(Copper, Duchy, Silver), deck = Deck(Province), discarded = Deck(Estate))

    subject.victories should contain only(Duchy, Estate, Province)
  }
}
