package com.github.fedragon.dominion

class CardsSpec extends UnitSpec {

  import KingdomCards._
  import TreasureCards._
  import VictoryCards._

  "Cellar" should "translate to: +1 action, discard N cards and draw N cards" in {
    val stateOne = Player("P", Deck(Cellar, Market), Deck(Copper))
    val stateTwo = stateOne.plays(Cellar)

    stateTwo.hand.cards should contain only Copper
    stateTwo.discarded.cards should contain only (Cellar, Market)
    stateTwo.deck.cards shouldBe 'empty
    stateTwo.turn shouldBe Turn(actions = 1, buys = 1, coins = 0)
  }

  "Market" should "translate to: +1 card, +1 action, +1 buy, +1 coin" in {
    val stateOne = Player("P", Deck(Market), Deck(Copper))
    val stateTwo = stateOne.plays(Market)

    stateTwo.hand.cards should contain only Copper
    stateTwo.deck.cards shouldBe 'empty
    stateTwo.turn shouldBe Turn(actions = 1, buys = 2, coins = 1)
  }

  "Smithy" should "translate to: + 3 cards" in {
    val stateOne = Player("P", Deck(Smithy), Deck(Copper, Estate, Copper))
    val stateTwo = stateOne.plays(Smithy)

    stateTwo.hand.cards.size shouldBe 3
    stateTwo.deck.cards shouldBe 'empty
  }
}
