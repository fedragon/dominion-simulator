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
    stateOne.hand.cards should contain(Copper)
    stateOne.deck.cards should contain only Estate

    val stateTwo = stateOne.draws
    stateTwo.deck.cards shouldBe 'empty
    stateTwo.hand.cards should contain only(Copper, Estate)
  }

  it should "be able to discard his.hand.cards" in {
    val stateOne = new Player("Player", deck = Deck(Copper, Estate)).draws
    stateOne.hand.cards should contain(Copper)

    val stateTwo = stateOne.discardHand
    stateTwo.hand.cards shouldBe 'empty
    stateTwo.discarded.cards should contain(Copper)
  }

  it should "play an action, if there is at least one such action in his.hand.cards" in {
    val subject = Player("Player", hand = Deck(Smithy), deck = Deck(Copper, Estate, Copper))

    val (player, _) = subject.plays(Smithy)(Game(Map(subject.name -> subject), EmptyDeck, EmptyDeck))

    player.hand.cards.size shouldBe 3
    player.deck.cards shouldBe 'empty
  }

  it should "not play an action, if it is not in his.hand.cards" in {
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
