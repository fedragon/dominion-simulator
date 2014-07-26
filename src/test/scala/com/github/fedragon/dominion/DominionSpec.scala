package com.github.fedragon.dominion

class DominionSpec extends UnitSpec {

  import KingdomCards._
  import TreasureCards._
  import VictoryCards._

  "Drawing cards from a deck" should "reduce the number of remaining cards" in {
    val deck = Deck(Cards(Copper))
    deck.cards.size shouldBe 1
    val (_, newDeck) = deck.draw.get

    newDeck.cards shouldBe 'empty
  }

  "A player" should "be able to buy a card if he has enough coins" in {
    val player = Player("Player", fillWith(7)(Copper), ZeroCards, Deck(ZeroCards))
    player.canBuy(Estate) shouldBe true

    val poor = Player("Poor", fillWith(1)(Copper), ZeroCards, Deck(ZeroCards))
    poor.canBuy(Estate) shouldBe false
  }

  it should "be able to draw from his deck" in {
    val stateOne = new Player("Player", deck = Deck(Cards(Copper, Estate))).draw
    stateOne.hand should contain (Copper)
    stateOne.deck.cards should contain only Estate

    val stateTwo = stateOne.draw
    stateTwo.deck.cards shouldBe 'empty
    stateTwo.hand should contain only (Copper, Estate)
  }

  it should "be able to discard his hand" in {
    val stateOne = new Player("Player", deck = Deck(Cards(Copper, Estate))).draw
    stateOne.hand should contain(Copper)

    val stateTwo = stateOne.discardHand
    stateTwo.hand shouldBe 'empty
    stateTwo.discarded should contain(Copper)
  }

  it should "play an action, if there is at least one such action in his hand" in {
    val stateOne = Player("Player", Cards(Smithy), Deck(Cards(Copper, Estate, Copper)))
    val stateTwo = stateOne.plays(Smithy)

    stateTwo.hand.size shouldBe 3
    stateTwo.deck.cards shouldBe 'empty
  }

  it should "not play an action, if it is not in his hand" in {
    val player = new Player("Player", deck = Deck(Cards(Copper, Estate)))
    player.plays(Smithy) shouldBe player
  }

  it should "not be allowed to play more actions than he can" in {
    val stateOne = Player("Player", Cards(Smithy), Deck(Cards(Copper, Estate, Copper)))
    val stateTwo = stateOne.plays(Smithy)
    val stateThree = stateTwo.plays(Smithy)

    stateOne should not equal stateTwo
    stateTwo shouldEqual stateThree
  }
}
