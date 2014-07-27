package com.github.fedragon.dominion

class DominionSpec extends UnitSpec {

  import Deck._
  import KingdomCards._
  import TreasureCards._
  import VictoryCards._

  "Drawing from a deck" should "reduce the number of remaining cards" in {
    val deck = Deck(Copper)
    deck.size shouldBe 1
    val (_, newDeck) = deck.draw.get

    newDeck shouldBe 'empty
  }

  "A player" should "be able to buy a card if he has enough coins" in {
    val player = Player("Player", fillWith(7)(Copper), EmptyDeck, EmptyDeck)
    player.canBuy(Estate) shouldBe true

    val poor = Player("Poor", fillWith(1)(Copper), EmptyDeck, EmptyDeck)
    poor.canBuy(Estate) shouldBe false
  }

  it should "be able to draw  from his deck" in {
    val stateOne = new Player("Player", deck = Deck(Copper, Estate)).draws
    stateOne.hand should contain(Copper)
    stateOne.deck should contain only Estate

    val stateTwo = stateOne.draws
    stateTwo.deck shouldBe 'empty
    stateTwo.hand should contain only(Copper, Estate)
  }

  it should "be able to discard his hand" in {
    val stateOne = new Player("Player", deck = Deck(Copper, Estate)).draws
    stateOne.hand should contain(Copper)

    val stateTwo = stateOne.discardHand
    stateTwo.hand shouldBe 'empty
    stateTwo.discarded should contain(Copper)
  }

  it should "play an action, if there is at least one such action in his hand" in {
    val subject = Player("Player", hand = Deck(Smithy), deck = Deck(Copper, Estate, Copper))

    val (player, _) = subject.plays(Smithy)(Game(Map(subject.name -> subject), EmptyDeck, EmptyDeck))

    player.hand.size shouldBe 3
    player.deck shouldBe 'empty
  }

  it should "not play an action, if it is not in his hand" in {
    val subject = new Player("Player", deck = Deck(Copper, Estate))

    val (player, _) = subject.plays(Smithy)(Game(Map(subject.name -> subject), EmptyDeck, EmptyDeck))

    player shouldBe subject
  }

  it should "not be allowed to play more actions than he can" in {
    val subject = Player("Player", hand = Deck(Smithy), deck = Deck(Copper, Estate, Copper))
    val game = Game(Map(subject.name -> subject), EmptyDeck, EmptyDeck)

    // Smithy does not affect `game` so there's no problem reusing the same instance
    val (stateOne, _) = subject.plays(Smithy)(game)
    val (stateTwo, _) = stateOne.plays(Smithy)(game)

    stateOne should not equal subject
    stateTwo shouldEqual stateOne
  }
}
