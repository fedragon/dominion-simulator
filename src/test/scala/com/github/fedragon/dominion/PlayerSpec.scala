package com.github.fedragon.dominion

import Deck._
import ActionCards._
import TreasureCards._
import VictoryCards._

class PlayerSpec extends UnitSpec {

  val emptyGame = Game(Map.empty, Map.empty, EmptyDeck)

  "A player" should "be able to draw one card from his deck" in {
    val pStateOne = new Player("X", deck = Deck(Copper, Estate)).draws
    pStateOne.hand should contain(Copper)
    pStateOne.deck.loneElement shouldBe Estate

    val pStateTwo = pStateOne.draws
    pStateTwo.deck shouldBe 'empty
    pStateTwo.hand should contain theSameElementsAs Deck(Copper, Estate)
  }

  it should "be able to draw N cards from his deck" in {
    val pStateOne = new Player("X", deck = Deck(Copper, Estate)).drawsN(2)

    pStateOne.deck shouldBe 'empty
    pStateOne.hand should contain theSameElementsAs Deck(Copper, Estate)
  }

  it should "be able to shuffle his cards before drawing, if needed" in {
    val pStateOne = new Player("X", deck = EmptyDeck, discarded = Deck(Copper)).draws

    pStateOne.deck shouldBe 'empty
    pStateOne.hand.loneElement shouldBe Copper
    pStateOne.discarded shouldBe 'empty
  }

  it should "be able to reveal and discard the top card in his deck" in {
    val subject = new Player("X", deck = Deck(Gold, Estate))
    val pStateOne = subject.reveals(_ => false)

    pStateOne.deck should contain theSameElementsAs Deck(Gold, Estate)
    pStateOne.discarded shouldBe 'empty

    val pStateTwo = subject.reveals(_ => true)

    pStateTwo.deck.loneElement shouldBe Estate
    pStateTwo.discarded.loneElement shouldBe Gold
  }

  it should "be able to reveal the top card in his deck even when all his cards are discarded" in {
    val subject = new Player("X", deck = EmptyDeck, discarded = Deck(Gold, Estate))
    val pStateOne = subject.reveals(_ => false)

    pStateOne.deck should contain theSameElementsAs Deck(Gold, Estate)
    pStateOne.discarded shouldBe 'empty

    val pStateTwo = subject.reveals(_ => true)

    // shuffling involved, cannot assert on specific cards
    pStateTwo.deck.size shouldBe 1
    pStateTwo.discarded.size shouldBe 1
  }

  it should "be able to reveal cards until a certain condition applies" in {
    val subject = Player("X", hand = Deck(Adventurer), deck = Deck(Market, Bureaucrat, Copper, Smithy, Silver, Province))

    val (cards, pStateOne) = subject.revealsUntil(_.onlyTreasures.size === 2)

    cards should contain theSameElementsAs Deck(Market, Bureaucrat, Smithy, Copper, Silver)
    pStateOne.deck.loneElement shouldBe Province
  }

  it should "be able to know the total amount of coins he can spend" in {
    val subject = new Player("X", hand = Deck(Copper, Silver), deck = EmptyDeck, turn = Turn(1, 1, Coins(2)))

    subject.allCoins shouldBe Coins(5)
  }

  it should "be able to discard one card from his hand" in {
    val subject = new Player("X", hand = Deck(Copper), deck = EmptyDeck)

    val pStateOne = subject.discard(Copper)

    pStateOne.hand shouldBe 'empty
    pStateOne.discarded.loneElement shouldBe Copper
  }

  it should "be able to discard N cards from his hand" in {
    val subject = new Player("X", hand = Deck(Copper, Silver), deck = EmptyDeck)

    val pStateOne = subject.discard(Deck(Copper, Silver))

    pStateOne.hand shouldBe 'empty
    pStateOne.discarded should contain theSameElementsAs Deck(Copper, Silver)
  }

  it should "be able to discard his whole hand" in {
    val subject = new Player("X", deck = Deck(Copper, Estate)).draws

    subject.hand.loneElement shouldBe Copper

    val pStateOne = subject.discardHand

    pStateOne.hand shouldBe 'empty
    pStateOne.discarded.loneElement shouldBe Copper
  }

  it should "play an action, if there is at least one such action in his hand" in {
    val subject = Player("X", hand = Deck(Smithy), deck = Deck(Copper, Estate, Copper))
    val game = emptyGame.copy(players = Map(subject.name -> subject))

    val (player, _) = subject.plays(Smithy)(game)

    player.hand.size shouldBe 3
    player.deck shouldBe 'empty
  }

  it should "not play an action, if it is not in his hand" in {
    val subject = new Player("X", deck = Deck(Copper, Estate))
    val game = emptyGame.copy(players = Map(subject.name -> subject))

    val (player, _) = subject.plays(Smithy)(game)

    player shouldBe subject
  }

  it should "not be allowed to play more actions than he can" in {
    val subject = Player("X", hand = Deck(Smithy), deck = Deck(Copper, Estate, Copper))
    val game = emptyGame.copy(players = Map(subject.name -> subject))

    // Smithy does not affect `game` so there's no problem reusing the same instance of game
    val (pStateOne, _) = subject.plays(Smithy)(game)
    val (pStateTwo, _) = pStateOne.plays(Smithy)(game)

    pStateOne should not equal subject
    pStateTwo shouldEqual pStateOne
  }

  it should "not be able to buy a card, if he does not have enough coins" in {
    val subject = Player("X", hand = Deck(Copper), deck = EmptyDeck)
    val game = emptyGame.copy(players = Map(subject.name -> subject), supplyPiles = Map(Moat -> 1))

    val (pStateOne, gStateOne) = subject.buys(Moat)(game)

    pStateOne._buys.get shouldBe 1
    pStateOne.hand.loneElement shouldBe Copper
    gStateOne.supplyPiles.loneElement shouldBe (Moat -> 1)
  }

  it should "be able to buy a card, if he has enough coins in his hand" in {
    val subject = Player("X", hand = Deck(Silver), deck = EmptyDeck)
    val game = emptyGame.copy(players = Map(subject.name -> subject), supplyPiles = Map(Moat -> 1))

    val (pStateOne, gStateOne) = subject.buys(Moat)(game)

    pStateOne._buys.get shouldBe 0
    pStateOne.hand.loneElement shouldBe Moat
    gStateOne.supplyPiles.loneElement shouldBe (Moat -> 0)
  }

  it should "be able to buy a card, if he has enough coins including extra coins" in {
    val subject = Player("X", hand = Deck(Copper), deck = EmptyDeck, turn = Turn(0, 1, Coins(1)))
    val game = emptyGame.copy(players = Map(subject.name -> subject), supplyPiles = Map(Moat -> 1))

    val (pStateOne, gStateOne) = subject.buys(Moat)(game)

    pStateOne._buys.get shouldBe 0
    pStateOne._coins.get shouldBe Coins(0)
    pStateOne.hand.loneElement shouldBe Moat
    gStateOne.supplyPiles.loneElement shouldBe (Moat -> 0)
  }

  it should "not be able to buy a card, if it is not available" in {
    val subject = Player("X", hand = Deck(Copper), deck = EmptyDeck, turn = Turn(0, 1, Coins(1)))
    val game = emptyGame.copy(players = Map(subject.name -> subject), supplyPiles = Map(Moat -> 0))

    val (pStateOne, gStateOne) = subject.buys(Moat)(game)

    pStateOne._buys.get shouldBe 1
    pStateOne._coins.get shouldBe Coins(1)
    pStateOne.hand.loneElement shouldBe Copper
    gStateOne.supplyPiles.loneElement shouldBe (Moat -> 0)
  }

  it should "be able to know all the victory cards in his hand, discarded pile, and deck" in {
    val subject = new Player("X", hand = Deck(Copper, Duchy, Silver), deck = Deck(Province), discarded = Deck(Estate))

    subject.allVictories should contain theSameElementsAs Deck(Duchy, Estate, Province)
  }

  it should "be able to play his starting turn and buy a card with his available coins" in {
    val startingHand = Deck(Copper, Copper, Estate, Estate, Estate)
    val startingDeck = Deck(Copper, Copper, Copper, Copper, Copper)

    val subject = new Player("X", hand = startingHand, deck = startingDeck, discarded = EmptyDeck)
    val g = Game(Map(subject.name -> subject), Map(Cellar -> 1), EmptyDeck)

    val gameOne = subject.playTurn(g)
    val stateOne = gameOne.find(subject)

    stateOne._actions.get shouldBe 0
    stateOne._buys.get shouldBe 0
    stateOne._coins.get shouldBe Coins(0)

    stateOne.hand should contain theSameElementsAs startingDeck
    stateOne.deck shouldBe 'empty
    stateOne.discarded should contain theSameElementsAs Cellar +: startingHand

    gameOne.supplyPiles should contain theSameElementsAs Map(Cellar -> 0)
    gameOne.trashed shouldBe 'empty
  }
}
