package com.github.fedragon.dominion

import Deck._
import KingdomCards._
import TreasureCards._
import VictoryCards._

class CardsSpec extends UnitSpec {

  val emptyGame = Game(Map.empty, Map.empty, EmptyDeck)

  "Cellar" should "translate to: +1 action, discard N cards and draw N cards" in {
    val subject = Player("P", hand = Deck(Cellar, Market), deck = Deck(Copper))
    val game = emptyGame.copy(players = Map(subject.name -> subject))

    val (stateOne, _) = subject.plays(Cellar)(game)

    stateOne.hand.loneElement shouldBe Copper
    stateOne.discarded should contain theSameElementsAs Deck(Cellar, Market)
    stateOne.deck shouldBe 'empty
    stateOne.turn shouldBe Turn(actions = 1, buys = 1, coins = Coins(0))
  }

  "Market" should "translate to: +1 card, +1 action, +1 buy, +1 coin" in {
    val subject = Player("P", hand = Deck(Market), deck = Deck(Copper))
    val game = emptyGame.copy(players = Map(subject.name -> subject))

    val (stateOne, _) = subject.plays(Market)(game)

    stateOne.hand.loneElement shouldBe Copper
    stateOne.deck shouldBe 'empty
    stateOne.turn shouldBe Turn(actions = 1, buys = 2, coins = Coins(1))
  }

  "Mine" should "translate to: trash 1 treasure card and get 1 whose cost is +3" in {
    val subject = Player("P", hand = Deck(Mine, Copper), deck = EmptyDeck)
    val game = emptyGame.copy(players = Map(subject.name -> subject), supplyPiles = Map(Silver -> 1))

    val (stateOne, gameOne) = subject.plays(Mine)(game)

    stateOne.hand.loneElement shouldBe Silver
    stateOne.deck shouldBe 'empty

    gameOne.trashed.loneElement shouldBe Copper
  }

  "Moat" should "translate to: +2 cards (when played as action)" in {
    val subject = Player("P", hand = Deck(Moat), deck = Deck(Copper, Copper))
    val game = emptyGame.copy(players = Map(subject.name -> subject))

    val (stateOne, _) = subject.plays(Moat)(game)

    stateOne.hand.size shouldBe 2
    stateOne.deck shouldBe 'empty
  }

  "Moat" should "nullify any attack on a player who has it in his hand" in {
    val subject = Player("P", hand = Deck(Witch), deck = Deck(Copper, Copper))
    val other = Player("O", hand = Deck(Moat), deck = EmptyDeck)
    val game = emptyGame.copy(players = Map(subject.name -> subject, other.name -> other))

    val (_, gameOne) = subject.plays(Witch)(game)

    val otherOne = gameOne.find(other)
    otherOne.deck shouldNot contain(Curse)
    otherOne.hand.loneElement shouldBe Moat
  }

  "Smithy" should "translate to: +3 cards" in {
    val subject = Player("P", hand = Deck(Smithy), deck = Deck(Copper, Estate, Copper))
    val game = emptyGame.copy(players = Map(subject.name -> subject))

    val (stateOne, _) = subject.plays(Smithy)(game)

    stateOne.hand.size shouldBe 3
    stateOne.deck shouldBe 'empty
  }

  "Spy" should "translate to: +1 card, +1 action, every player reveals his top cards and maybe discards it, the attacker decides" in {
    class MyStrategy extends DefaultStrategy {
      override def shouldSpyHolderDiscard(card: Card) = true
      override def shouldSpyVictimDiscard(card: Card) = card === Copper
    }

    val subject = new Player("P", hand = Deck(Spy), deck = Deck(Estate, Gold), strategy = new MyStrategy)
    val other = Player("O", hand = EmptyDeck, deck = Deck(Copper, Smithy))
    val another = Player("A", hand = EmptyDeck, deck = EmptyDeck, discarded = Deck(Mine, Province))
    val game = emptyGame.copy(players = Map(subject.name -> subject, other.name -> other, another.name -> another))

    val (stateOne, gameOne) = subject.plays(Spy)(game)

    stateOne.hand.loneElement shouldBe Estate
    stateOne.turn shouldBe Turn(actions = 1, buys = 1, coins = Coins(0))
    stateOne.discarded should contain theSameElementsAs Deck(Gold, Spy)

    gameOne.find(other).deck.loneElement shouldBe Smithy
    gameOne.find(other).discarded.loneElement shouldBe Copper

    gameOne.find(another).deck should contain theSameElementsAs Deck(Mine, Province)
    gameOne.find(another).discarded shouldBe 'empty
  }

  "Witch" should "translate to: +2 and +1 curse to all the other players" in {
    val subject = Player("P", hand = Deck(Witch), deck = Deck(Copper, Copper))
    val other = Player("O", hand = EmptyDeck, deck = EmptyDeck)
    val another = Player("A", hand = EmptyDeck, deck = EmptyDeck)
    val game = emptyGame.copy(players = Map(subject.name -> subject, other.name -> other, another.name -> another), supplyPiles = Map(Curse -> 2))

    val (stateOne, gameOne) = subject.plays(Witch)(game)

    stateOne.hand.size shouldBe 2
    stateOne.deck shouldBe 'empty

    gameOne.find(other).deck.loneElement shouldBe Curse
    gameOne.find(another).deck.loneElement shouldBe Curse
  }
}
