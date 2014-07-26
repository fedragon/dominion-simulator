package com.github.fedragon.dominion

class DominionSpec extends UnitSpec {

  import Deck._
  import KingdomCards._
  import TreasureCards._
  import VictoryCards._

  "Drawing cards from a deck" should "reduce the number of remaining cards" in {
    val deck = Deck(Copper)
    deck.cards.size shouldBe 1
    val (_, newDeck) = deck.draw.get

    newDeck.cards shouldBe 'empty
  }

  "A player" should "be able to buy a card if he has enough coins" in {
    val player = Player("Player", fillWith(7)(Copper), EmptyDeck, EmptyDeck)
    player.canBuy(Estate) shouldBe true

    val poor = Player("Poor", fillWith(1)(Copper), EmptyDeck, EmptyDeck)
    poor.canBuy(Estate) shouldBe false
  }

  it should "be able to.draws from his deck" in {
    val stateOne = new Player("Player", deck = Deck(Copper, Estate)).draws
    stateOne.hand.cards should contain (Copper)
    stateOne.deck.cards should contain only Estate

    val stateTwo = stateOne.draws
    stateTwo.deck.cards shouldBe 'empty
    stateTwo.hand.cards should contain only (Copper, Estate)
  }

  it should "be able to discard his.hand.cards" in {
    val stateOne = new Player("Player", deck = Deck(Copper, Estate)).draws
    stateOne.hand.cards should contain(Copper)

    val stateTwo = stateOne.discardHand
    stateTwo.hand.cards shouldBe 'empty
    stateTwo.discarded.cards should contain(Copper)
  }

  it should "play an action, if there is at least one such action in his.hand.cards" in {
    val stateOne = Player("Player", Deck(Smithy), Deck(Copper, Estate, Copper))
    val stateTwo = stateOne.plays(Smithy)

    stateTwo.hand.cards.size shouldBe 3
    stateTwo.deck.cards shouldBe 'empty
  }

  it should "not play an action, if it is not in his.hand.cards" in {
    val player = new Player("Player", deck = Deck(Copper, Estate))
    player.plays(Smithy) shouldBe player
  }

  it should "not be allowed to play more actions than he can" in {
    val stateOne = Player("Player", Deck(Smithy), Deck(Copper, Estate, Copper))
    val stateTwo = stateOne.plays(Smithy)
    val stateThree = stateTwo.plays(Smithy)

    stateOne should not equal stateTwo
    stateTwo shouldEqual stateThree
  }
}
