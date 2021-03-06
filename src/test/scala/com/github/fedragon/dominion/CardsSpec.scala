package com.github.fedragon.dominion

import Deck._
import ActionCards._
import TreasureCards._
import VictoryCards._

class CardsSpec extends UnitSpec {

  val emptyGame = Game(Map.empty, Map.empty, EmptyDeck)

  "Adventurer" should "translate to: reveal cards from deck until you draw 2 treasures; discard any other revealed card" in {
    val subject = Player("X", hand = Deck(Adventurer), deck = Deck(Market, Bureaucrat, Copper, Smithy, Silver, Province))
    val other = Player("Y", hand = EmptyDeck, deck = EmptyDeck)
    val game = emptyGame.copy(players = Map(subject.name -> subject, other.name -> other), supplyPiles = Map.empty)

    val (stateOne, _) = subject.plays(Adventurer)(game)

    stateOne.hand should contain theSameElementsAs Deck(Copper, Silver)
    stateOne.deck.loneElement shouldBe Province
    stateOne.discarded should contain theSameElementsAs Deck(Adventurer, Market, Bureaucrat, Smithy)
    stateOne.turn shouldBe Turn(actions = 0, buys = 1, coins = Coins(0))
  }

  "Bureaucrat" should "translate to: gain 1 Silver on top of your deck, victims reveal a Victory card and put it on top of their deck" in {
    val subject = Player("X", hand = Deck(Bureaucrat), deck = EmptyDeck)
    val other = Player("Y", hand = Deck(Copper, Market, Province), deck = Deck(Market))
    val game = emptyGame.copy(players = Map(subject.name -> subject, other.name -> other), supplyPiles = Map(Silver -> 1))

    val (stateOne, gameOne) = subject.plays(Bureaucrat)(game)

    stateOne.deck.head shouldBe Silver
    stateOne.turn shouldBe Turn(actions = 0, buys = 1, coins = Coins(0))
    stateOne.discarded.loneElement shouldBe Bureaucrat

    gameOne.find(other).deck.head shouldBe Province
  }

  "Cellar" should "translate to: +1 action, discard N cards and draw N cards" in {
    val subject = Player("X", hand = Deck(Cellar, Province), deck = Deck(Copper))
    val game = emptyGame.copy(players = Map(subject.name -> subject))

    val (stateOne, _) = subject.plays(Cellar)(game)

    stateOne.hand.loneElement shouldBe Copper
    stateOne.discarded should contain theSameElementsAs Deck(Cellar, Province)
    stateOne.deck shouldBe 'empty
    stateOne.turn shouldBe Turn(actions = 1, buys = 1, coins = Coins(0))
  }

  "Chancellor" should "translate to: +2 coins, you may put your deck into your discarded pile" in {
    val subject = Player("X", hand = Deck(Chancellor), deck = Deck(Curse, Curse, Province))
    val game = emptyGame.copy(players = Map(subject.name -> subject))

    val (stateOne, gameOne) = subject.plays(Chancellor)(game)

    stateOne.hand shouldBe 'empty
    stateOne.discarded should contain theSameElementsAs Deck(Chancellor, Curse, Curse, Province)
    stateOne.deck shouldBe 'empty
    stateOne.turn shouldBe Turn(0, 1, Coins(2))
  }

  "Chapel" should "translate to: trash up to 4 cards" in {
    val subject = Player("X", hand = Deck(Chapel, Curse, Curse, Province), deck = EmptyDeck)
    val game = emptyGame.copy(players = Map(subject.name -> subject))

    val (stateOne, gameOne) = subject.plays(Chapel)(game)

    stateOne.hand.loneElement shouldBe Province
    stateOne.discarded.loneElement shouldBe Chapel
    stateOne.deck shouldBe 'empty
    stateOne.turn shouldBe Turn(actions = 0, buys = 1, coins = Coins(0))

    gameOne.trashed should contain theSameElementsAs Deck(Curse, Curse)
  }

  "Council room" should "translate to: +4 cards, +1 buy, every other player draws one card" in {
    val subject = Player("X", hand = Deck(CouncilRoom), deck = Deck(Copper, Market, Smithy, Witch))
    val other = Player("Y", hand = EmptyDeck, deck = Deck(Moat))
    val game = emptyGame.copy(players = Map(subject.name -> subject, other.name -> other))

    val (stateOne, gameOne) = subject.plays(CouncilRoom)(game)

    stateOne.hand should contain theSameElementsAs Deck(Copper, Market, Smithy, Witch)
    stateOne.discarded.loneElement shouldBe CouncilRoom
    stateOne.turn shouldBe Turn(actions = 0, buys = 2, coins = Coins(0))

    gameOne.find(other).hand should contain theSameElementsAs Deck(Moat)
  }

  "Feast" should "translate to: trash this card to gain one costing up to 5 coins" in {
    val subject = Player("X", hand = Deck(Feast), deck = EmptyDeck)
    val game = emptyGame.copy(players = Map(subject.name -> subject), supplyPiles = Map(Chapel -> 1, Witch -> 1))

    val (stateOne, gameOne) = subject.plays(Feast)(game)

    stateOne.hand shouldBe 'empty
    stateOne.discarded.loneElement shouldBe Witch
    stateOne.deck shouldBe 'empty
    stateOne.turn shouldBe Turn(actions = 0, buys = 1, coins = Coins(0))

    gameOne.trashed.loneElement shouldBe Feast
  }

  "Festival" should "translate to: +2 actions, +1 buy, +2 coins" in {
    val subject = Player("X", hand = Deck(Festival), deck = EmptyDeck)
    val game = emptyGame.copy(players = Map(subject.name -> subject))

    val (stateOne, _) = subject.plays(Festival)(game)

    stateOne.hand shouldBe 'empty
    stateOne.discarded.loneElement shouldBe Festival
    stateOne.deck shouldBe 'empty
    stateOne.turn shouldBe Turn(2, 2, Coins(2))
  }

  "Gardens" should "translate to: +1 point for every 10 cards, rounded down" in {
    Estate.value(0) shouldBe 1

    Gardens.value(5) shouldBe 0
    Gardens.value(16) shouldBe 1
    Gardens.value(30) shouldBe 3

    (Estate.value(0) + Gardens.value(15)) shouldBe 2
    (Gardens.value(30) + Gardens.value(5)) shouldBe 3
  }

  "Laboratory" should "translate to: +2 cards, +1 action" in {
    val subject = Player("X", hand = Deck(Laboratory), deck = Deck(Province, Silver))
    val game = emptyGame.copy(players = Map(subject.name -> subject))

    val (stateOne, _) = subject.plays(Laboratory)(game)

    stateOne.hand should contain theSameElementsAs Deck(Province, Silver)
    stateOne.deck shouldBe 'empty
    stateOne.discarded.loneElement shouldBe Laboratory
    stateOne.turn shouldBe Turn(actions = 1, buys = 1, coins = Coins(0))
  }

  "Library" should "translate to: draw until you have 7 cards in your hand, you may discard any Action card you draw" in {
    val subject = Player("X", hand = Deck(Library, Laboratory, Province, Silver, Moat), deck = Deck(Workshop, Spy, Province, Silver))
    val game = emptyGame.copy(players = Map(subject.name -> subject))

    val (stateOne, _) = subject.plays(Library)(game)

    stateOne.hand.size shouldBe 7
    stateOne.discarded.loneElement shouldBe Library
    stateOne.turn shouldBe Turn(actions = 0, buys = 1, coins = Coins(0))
  }

  "Market" should "translate to: +1 card, +1 action, +1 buy, +1 coin" in {
    val subject = Player("X", hand = Deck(Market), deck = Deck(Copper))
    val game = emptyGame.copy(players = Map(subject.name -> subject))

    val (stateOne, _) = subject.plays(Market)(game)

    stateOne.hand.loneElement shouldBe Copper
    stateOne.deck shouldBe 'empty
    stateOne.discarded.loneElement shouldBe Market
    stateOne.turn shouldBe Turn(actions = 1, buys = 2, coins = Coins(1))
  }

  "Militia" should "translate to: +2 coins, victims discard until they are left with 3 cards in their hand" in {
    val subject = Player("X", hand = Deck(Militia), deck = EmptyDeck)
    val other = Player("Y", hand = Deck(Smithy), deck = EmptyDeck)
    val another = Player("Z", hand = Deck(Copper, Market, Silver, Spy, Witch), deck = EmptyDeck)
    val game = emptyGame.copy(players = Map(subject.name -> subject, other.name -> other, another.name -> another))

    val (stateOne, gameOne) = subject.plays(Militia)(game)

    stateOne.discarded.loneElement shouldBe Militia
    stateOne.turn shouldBe Turn(actions = 0, buys = 1, coins = Coins(2))

    gameOne.find(other).hand should contain theSameElementsAs Deck(Smithy)
    gameOne.find(another).hand should contain theSameElementsAs Deck(Silver, Spy, Witch)
  }

  "Mine" should "translate to: trash 1 Treasure and get 1 whose cost is +3" in {
    val subject = Player("X", hand = Deck(Mine, Copper), deck = EmptyDeck)
    val game = emptyGame.copy(players = Map(subject.name -> subject), supplyPiles = Map(Silver -> 1))

    val (stateOne, gameOne) = subject.plays(Mine)(game)

    stateOne.hand.loneElement shouldBe Silver
    stateOne.deck shouldBe 'empty
    stateOne.discarded.loneElement shouldBe Mine

    gameOne.trashed.loneElement shouldBe Copper
  }

  "Moat" should "translate to: +2 cards (when played as action)" in {
    val subject = Player("X", hand = Deck(Moat), deck = Deck(Copper, Copper))
    val game = emptyGame.copy(players = Map(subject.name -> subject))

    val (stateOne, _) = subject.plays(Moat)(game)

    stateOne.hand.size shouldBe 2
    stateOne.deck shouldBe 'empty
    stateOne.discarded.loneElement shouldBe Moat
  }

  it should "nullify any attack on a player who has it in his hand" in {
    val subject = Player("X", hand = Deck(Witch), deck = Deck(Copper, Copper))
    val other = Player("Y", hand = Deck(Moat), deck = EmptyDeck)
    val game = emptyGame.copy(players = Map(subject.name -> subject, other.name -> other))

    val (_, gameOne) = subject.plays(Witch)(game)

    val otherOne = gameOne.find(other)
    otherOne.deck shouldNot contain(Curse)
    otherOne.hand.loneElement shouldBe Moat
  }

  "Moneylender" should "translate to: trash a Copper and gain +3 coins" in {
    val subject = Player("X", hand = Deck(Copper, Moneylender), deck = EmptyDeck)
    val game = emptyGame.copy(players = Map(subject.name -> subject))

    val (stateOne, gameOne) = subject.plays(Moneylender)(game)

    stateOne.hand shouldBe 'empty
    stateOne.discarded.loneElement shouldBe Moneylender
    stateOne.deck shouldBe 'empty
    stateOne.turn shouldBe Turn(actions = 0, buys = 1, coins = Coins(3))

    gameOne.trashed.loneElement shouldBe Copper
  }

  "Remodel" should "translate to: trash a card from the hand and gain one that costs up to 2 coins more" in {
    val subject = Player("X", hand = Deck(Copper, Remodel), deck = EmptyDeck)
    val game = emptyGame.copy(players = Map(subject.name -> subject), supplyPiles = Map(Chapel -> 1))

    val (stateOne, gameOne) = subject.plays(Remodel)(game)

    stateOne.hand shouldBe 'empty
    stateOne.discarded should contain theSameElementsAs Deck(Remodel, Chapel)
    stateOne.deck shouldBe 'empty

    gameOne.trashed.loneElement shouldBe Copper
  }
  "Smithy" should "translate to: +3 cards" in {
    val subject = Player("X", hand = Deck(Smithy), deck = Deck(Copper, Estate, Copper))
    val game = emptyGame.copy(players = Map(subject.name -> subject))

    val (stateOne, _) = subject.plays(Smithy)(game)

    stateOne.hand.size shouldBe 3
    stateOne.deck shouldBe 'empty
    stateOne.discarded.loneElement shouldBe Smithy
  }

  "Spy" should "translate to: +1 card, +1 action, everybody reveals his top cards and maybe discards it, the attacker decides" in {
    class MyStrategy extends DefaultStrategy {
      override def spyHolderDiscards(card: Card) = true
      override def spyVictimDiscards(card: Card) = card === Copper
    }

    val subject = new Player("X", hand = Deck(Spy), deck = Deck(Estate, Gold), strategy = new MyStrategy)
    val other = Player("Y", hand = EmptyDeck, deck = Deck(Copper, Smithy))
    val another = Player("Z", hand = EmptyDeck, deck = EmptyDeck, discarded = Deck(Mine, Province))
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

  "Thief" should "translate to: every victim reveals 2 cards from his deck and attacker can gain or trash one of them (the remaining are discarded)" in {
    val subject = new Player("X", hand = Deck(Thief), deck = EmptyDeck)
    val other = Player("Y", hand = EmptyDeck, deck = Deck(Silver, Smithy))
    val another = Player("Z", hand = EmptyDeck, deck = EmptyDeck, discarded = Deck(Copper, Copper))
    val game = emptyGame.copy(players = Map(subject.name -> subject, other.name -> other, another.name -> another))

    val (stateOne, gameOne) = subject.plays(Thief)(game)

    stateOne.hand shouldBe 'empty
    stateOne.discarded should contain theSameElementsAs Deck(Thief, Silver)

    gameOne.find(other).deck shouldBe 'empty
    gameOne.find(other).discarded.loneElement shouldBe Smithy

    gameOne.find(another).deck shouldBe 'empty
    gameOne.find(another).discarded.loneElement shouldBe Copper

    gameOne.trashed.loneElement shouldBe Copper
  }

  "Throne room" should "translate to: choose an action from you hand and play it twice" in {
    val subject = new Player("X", hand = Deck(Market, ThroneRoom), deck = Deck(Smithy, Thief, Witch))
    val game = emptyGame.copy(players = Map(subject.name -> subject))

    val (stateOne, _) = subject.plays(ThroneRoom)(game)

    stateOne.hand should contain theSameElementsAs Deck(Smithy, Thief)
    stateOne.discarded should contain theSameElementsAs Deck(Market, ThroneRoom)
    stateOne.turn shouldBe Turn(2, 3, Coins(2))
  }

  "Village" should "translate to: draw 1 card, +2 actions" in {
    val subject = Player("X", hand = Deck(Village), deck = Deck(Smithy))
    val game = emptyGame.copy(players = Map(subject.name -> subject))

    val (stateOne, _) = subject.plays(Village)(game)

    stateOne.hand.loneElement shouldBe Smithy
    stateOne.discarded.loneElement shouldBe Village
    stateOne.turn shouldBe Turn(actions = 2, buys = 1, coins = Coins(0))
  }

  "Witch" should "translate to: +2 and +1 curse to all the victims" in {
    val subject = Player("X", hand = Deck(Witch), deck = Deck(Copper, Copper))
    val other = Player("Y", hand = EmptyDeck, deck = EmptyDeck)
    val another = Player("Z", hand = EmptyDeck, deck = EmptyDeck)
    val game = emptyGame.copy(
      players = Map(subject.name -> subject, other.name -> other, another.name -> another),
      supplyPiles = Map(Smithy -> 2, Curse -> 5))

    val (stateOne, gameOne) = subject.plays(Witch)(game)

    stateOne.hand.size shouldBe 2
    stateOne.deck shouldBe 'empty
    stateOne.discarded.loneElement shouldBe Witch

    gameOne.find(other).deck.loneElement shouldBe Curse
    gameOne.find(another).deck.loneElement shouldBe Curse
  }

  it should "not give Curses to victims if their pile is empty" in {
    val subject = Player("X", hand = Deck(Witch), deck = Deck(Copper, Copper))
    val other = Player("Y", hand = EmptyDeck, deck = EmptyDeck)
    val another = Player("Z", hand = EmptyDeck, deck = EmptyDeck)
    val game = emptyGame.copy(
      players = Map(subject.name -> subject, other.name -> other, another.name -> another),
      supplyPiles = Map(Smithy -> 2, Curse -> 0))

    val (_, gameOne) = subject.plays(Witch)(game)

    gameOne.find(other).deck shouldBe 'empty
    gameOne.find(another).deck shouldBe 'empty
  }

  "Woodcutter" should "translate to: +1 buy, +2 coins" in {
    val subject = Player("X", hand = Deck(Woodcutter), deck = EmptyDeck)
    val game = emptyGame.copy(players = Map(subject.name -> subject))

    val (stateOne, _) = subject.plays(Woodcutter)(game)

    stateOne.hand shouldBe 'empty
    stateOne.discarded.loneElement shouldBe Woodcutter
    stateOne.turn shouldBe Turn(actions = 0, buys = 2, coins = Coins(2))
  }

  "Workshop" should "translate to: gain a card costing up to 4 coins" in {
    val subject = Player("X", hand = Deck(Workshop), deck = EmptyDeck)
    val game = emptyGame.copy(
      players = Map(subject.name -> subject),
      supplyPiles = Map(Smithy -> 2, Curse -> 5))

    val (stateOne, gameOne) = subject.plays(Workshop)(game)

    stateOne.hand shouldBe 'empty
    stateOne.discarded should contain theSameElementsAs Deck(Workshop, Smithy)

    gameOne.supplyPiles(Smithy) shouldBe 1
    gameOne.supplyPiles(Curse) shouldBe 5
  }
}
