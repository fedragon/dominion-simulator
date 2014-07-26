package com.github.fedragon.dominion

class CardsSpec extends UnitSpec {

  import KingdomCards._
  import TreasureCards._
  import VictoryCards._

  "Market" should "translate to: +1 card, +1 action, +1 buy, +1 coin" in {
    val stateOne = Player("P", hand = Cards(Market), deck = Deck(Cards(Copper)))
    val stateTwo = stateOne.plays(Market)

    stateTwo.hand should contain only Copper
    stateTwo.deck.cards shouldBe 'empty
    stateTwo.turn shouldBe Turn(actions = 1, buys = 2, coins = 1)
  }

  "Smithy" should "translate to: + 3 cards" in {
    val stateOne = Player("P", hand = Cards(Smithy), deck = Deck(Cards(Copper, Estate, Copper)))
    val stateTwo = stateOne.plays(Smithy)

    stateTwo.hand.size shouldBe 3
    stateTwo.deck.cards shouldBe 'empty
  }
}
