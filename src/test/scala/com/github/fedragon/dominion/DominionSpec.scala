package com.github.fedragon.dominion

class DominionSpec extends UnitSpec {

  import Dominion._
  import KingdomCards._
  import TreasureCards._
  import VictoryCards._

  "Drawing cards from a deck" should "reduce the number of remaining cards" in {
    val deck = Deck(Vector(Copper))
    deck.cards.size shouldBe 1
    val (_, newDeck) = deck.draw.get

    newDeck.cards shouldBe 'empty
  }

  "A player" should "be able to buy a card if he has enough coins" in {
    val player = Player("Player", Vector.fill(7)(Copper), Vector.empty, Deck(Vector.empty))
    player.canBuy(Estate) shouldBe true

    val poor = Player("Poor", Vector.fill(1)(Copper), Vector.empty, Deck(Vector.empty))
    poor.canBuy(Estate) shouldBe false
  }

  it should "be able to draw from his deck" in {
    val stateOne = new Player("Player", deck = Deck(Vector(Copper, Estate))).draw
    stateOne.hand should contain (Copper)
    stateOne.deck.cards should contain only Estate

    val stateTwo = stateOne.draw
    stateTwo.deck.cards shouldBe 'empty
    stateTwo.hand should contain only (Copper, Estate)
  }

  it should "be able to discard from his hand" in {
    val stateOne = new Player("Player", deck = Deck(Vector(Copper, Estate))).draw
    stateOne.hand should contain(Copper)

    val stateTwo = stateOne.discard
    stateTwo.hand shouldBe 'empty
    stateTwo.discarded should contain(Copper)
  }

  it should "play an action, if there is at least one in his hand" in {
    val stateOne = Player("Player", hand = Vector(Smithy), deck = Deck(Vector(Copper, Estate, Copper)))
    val stateTwo = stateOne.plays

    stateTwo.hand.size shouldBe 4
    stateTwo.deck.cards shouldBe 'empty
  }

  it should "not play an action, if there are not any in his hand" in {
    val player = new Player("Player", deck = Deck(Vector(Copper, Estate)))
    player.plays shouldBe player
  }

  it should "not be allowed to play more actions than he can" in {
    val stateOne = Player("Player", hand = Vector(Smithy), deck = Deck(Vector(Copper, Estate, Copper)))
    val stateTwo = stateOne.plays
    val stateThree = stateTwo.plays

    stateOne should not equal stateTwo
    stateTwo shouldEqual stateThree
  }
}
