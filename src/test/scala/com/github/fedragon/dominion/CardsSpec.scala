package com.github.fedragon.dominion

class CardsSpec extends UnitSpec {

  import Deck._
  import KingdomCards._
  import TreasureCards._
  import VictoryCards._

  "Cellar" should "translate to: +1 action, discard N cards and draw N cards" in {
    val subject = Player("P", hand = Deck(Cellar, Market), deck = Deck(Copper))
    val game = Game(Vector(subject), EmptyDeck, EmptyDeck)

    val (stateOne, _) = subject.plays(Cellar)(game)

    stateOne.hand.cards should contain only Copper
    stateOne.discarded.cards should contain only(Cellar, Market)
    stateOne.deck.cards shouldBe 'empty
    stateOne.turn shouldBe Turn(actions = 1, buys = 1, coins = 0)
  }

  "Market" should "translate to: +1 card, +1 action, +1 buy, +1 coin" in {
    val subject = Player("P", hand = Deck(Market), deck = Deck(Copper))
    val game = Game(Vector(subject), EmptyDeck, EmptyDeck)

    val (stateOne, _) = subject.plays(Market)(game)

    stateOne.hand.cards should contain only Copper
    stateOne.deck.cards shouldBe 'empty
    stateOne.turn shouldBe Turn(actions = 1, buys = 2, coins = 1)
  }

  "Mine" should "translate to: trash 1 treasure card and get 1 whose cost is +3" in {
    val subject = Player("P", hand = Deck(Mine, Copper), deck = EmptyDeck)
    val game = Game(Vector(subject), Deck(Silver), EmptyDeck)

    val (stateOne, updatedGame) = subject.plays(Mine)(game)

    stateOne.hand.cards should contain only Silver
    stateOne.deck.cards shouldBe 'empty

    updatedGame.trashed.cards should contain only Copper
  }

  "Smithy" should "translate to: + 3 cards" in {
    val subject = Player("P", hand = Deck(Smithy), deck = Deck(Copper, Estate, Copper))
    val game = Game(Vector(subject), EmptyDeck, EmptyDeck)

    val (stateOne, _) = subject.plays(Smithy)(game)

    stateOne.hand.cards.size shouldBe 3
    stateOne.deck.cards shouldBe 'empty
  }
}
