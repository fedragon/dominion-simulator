package com.github.fedragon.dominion

class CardsSpec extends UnitSpec {

  import Deck._
  import KingdomCards._
  import TreasureCards._
  import VictoryCards._

  "Cellar" should "translate to: +1 action, discard N cards and draw N cards" in {
    val subject = Player("P", hand = Deck(Cellar, Market), deck = Deck(Copper))
    val game = Game(Map(subject.name -> subject), EmptyDeck, EmptyDeck)

    val (stateOne, _) = subject.plays(Cellar)(game)

    stateOne.hand.cards should contain only Copper
    stateOne.discarded.cards should contain only(Cellar, Market)
    stateOne.deck.cards shouldBe 'empty
    stateOne.turn shouldBe Turn(actions = 1, buys = 1, coins = 0)
  }

  "Market" should "translate to: +1 card, +1 action, +1 buy, +1 coin" in {
    val subject = Player("P", hand = Deck(Market), deck = Deck(Copper))
    val game = Game(Map(subject.name -> subject), EmptyDeck, EmptyDeck)

    val (stateOne, _) = subject.plays(Market)(game)

    stateOne.hand.cards should contain only Copper
    stateOne.deck.cards shouldBe 'empty
    stateOne.turn shouldBe Turn(actions = 1, buys = 2, coins = 1)
  }

  "Mine" should "translate to: trash 1 treasure card and get 1 whose cost is +3" in {
    val subject = Player("P", hand = Deck(Mine, Copper), deck = EmptyDeck)
    val game = Game(Map(subject.name -> subject), Deck(Silver), EmptyDeck)

    val (stateOne, updatedGame) = subject.plays(Mine)(game)

    stateOne.hand.cards should contain only Silver
    stateOne.deck.cards shouldBe 'empty

    updatedGame.trashed.cards should contain only Copper
  }

  "Moat" should "translate to: +2 cards (when played as action)" in {
    val subject = Player("P", hand = Deck(Moat), deck = Deck(Copper, Copper))
    val game = Game(Map(subject.name -> subject), EmptyDeck, EmptyDeck)

    val (stateOne, _) = subject.plays(Moat)(game)

    stateOne.hand.cards.size shouldBe 2
    stateOne.deck.cards shouldBe 'empty
  }

  "Moat" should "nullify any attack on the player" in {
    val subject = Player("P", hand = Deck(Witch), deck = Deck(Copper, Copper))
    val other = Player("O", hand = Deck(Moat), deck = EmptyDeck)
    val game = Game(Map(subject.name -> subject, other.name -> other), EmptyDeck, EmptyDeck)

    val (_, gameOne) = subject.plays(Witch)(game)

    val otherOne = gameOne.find(other)
    otherOne.deck.cards shouldNot contain (Curse)
    otherOne.hand.cards should contain only Moat
  }

  "Smithy" should "translate to: +3 cards" in {
    val subject = Player("P", hand = Deck(Smithy), deck = Deck(Copper, Estate, Copper))
    val game = Game(Map(subject.name -> subject), EmptyDeck, EmptyDeck)

    val (stateOne, _) = subject.plays(Smithy)(game)

    stateOne.hand.cards.size shouldBe 3
    stateOne.deck.cards shouldBe 'empty
  }

  "Witch" should "translate to: +2 cards and +1 curse to all the other players" in {
    val subject = Player("P", hand = Deck(Witch), deck = Deck(Copper, Copper))
    val other = Player("O", hand = EmptyDeck, deck = EmptyDeck)
    val game = Game(Map(subject.name -> subject, other.name -> other), Deck(Curse), EmptyDeck)

    val (stateOne, gameOne) = subject.plays(Witch)(game)

    stateOne.hand.cards.size shouldBe 2
    stateOne.deck.cards shouldBe 'empty

    gameOne.find(other).deck.cards should contain only Curse
  }
}
