package com.github.fedragon.dominion

class CardsSpec extends UnitSpec {

  import Deck._
  import KingdomCards._
  import TreasureCards._
  import VictoryCards._

  val emptyGame = Game(Map.empty, Map.empty, EmptyDeck)

  "Cellar" should "translate to: +1 action, discard N cards and draw N cards" in {
    val subject = Player("P", hand = Deck(Cellar, Market), deck = Deck(Copper))
    val game = emptyGame.copy(players = Map(subject.name -> subject))

    val (stateOne, _) = subject.plays(Cellar)(game)

    stateOne.hand should contain only Copper
    stateOne.discarded should contain only(Cellar, Market)
    stateOne.deck shouldBe 'empty
    stateOne.turn shouldBe Turn(actions = 1, buys = 1, coins = Coins(0))
  }

  "Market" should "translate to: +1 card, +1 action, +1 buy, +1 coin" in {
    val subject = Player("P", hand = Deck(Market), deck = Deck(Copper))
    val game = emptyGame.copy(players = Map(subject.name -> subject))

    val (stateOne, _) = subject.plays(Market)(game)

    stateOne.hand should contain only Copper
    stateOne.deck shouldBe 'empty
    stateOne.turn shouldBe Turn(actions = 1, buys = 2, coins = Coins(1))
  }

  "Mine" should "translate to: trash 1 treasure card and get 1 whose cost is +3" in {
    val subject = Player("P", hand = Deck(Mine, Copper), deck = EmptyDeck)
    val game = emptyGame.copy(players = Map(subject.name -> subject), supplyPiles = Map(Silver -> 1))

    val (stateOne, updatedGame) = subject.plays(Mine)(game)

    stateOne.hand should contain only Silver
    stateOne.deck shouldBe 'empty

    updatedGame.trashed should contain only Copper
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
    otherOne.deck shouldNot contain (Curse)
    otherOne.hand should contain only Moat
  }

  "Smithy" should "translate to: +3 cards" in {
    val subject = Player("P", hand = Deck(Smithy), deck = Deck(Copper, Estate, Copper))
    val game = emptyGame.copy(players = Map(subject.name -> subject))

    val (stateOne, _) = subject.plays(Smithy)(game)

    stateOne.hand.size shouldBe 3
    stateOne.deck shouldBe 'empty
  }

  "Spy" should "translate to: +1 card, +1 action, every player reveals his top cards and maybe discards it, the attacker decides" in {
    val subject = Player("P", hand = Deck(Spy), deck = Deck(Estate, Copper))
    val other = Player("O", hand = EmptyDeck, deck = Deck(Copper, Smithy))
    val another = Player("A", hand = EmptyDeck, deck = Deck(Mine, Province))
    val game = emptyGame.copy(players = Map(subject.name -> subject, other.name -> other, another.name -> another))

    val (stateOne, gameOne) = subject.plays(Spy)(game)

    stateOne.hand.size shouldBe 1
    stateOne.turn shouldBe Turn(actions = 1, buys = 1, coins = Coins(0))
    stateOne.deck.head shouldBe Copper

    gameOne.find(other).deck.head shouldBe Smithy
    gameOne.find(other).discarded.head shouldBe Copper

    gameOne.find(another).deck.head shouldBe Mine
    gameOne.find(another).discarded shouldBe 'empty
  }

  it should "make sure that the victim puts the revealed card back on top of his deck, if the attacker says so" in {
    val subject = Player("P", hand = Deck(Spy), deck = Deck(Estate, Copper))
    val other = Player("O", hand = EmptyDeck, deck = Deck(Province, Gold))
    val game = emptyGame.copy(players = Map(subject.name -> subject, other.name -> other))

    val (_, gameOne) = subject.plays(Spy)(game)

    gameOne.find(other).deck.head shouldBe Province
    gameOne.find(other).discarded shouldBe 'empty
  }

  it should "make sure that the victim shuffles his deck, if he does not have any card left" in {
    val subject = Player("P", hand = Deck(Spy), deck = Deck(Estate, Copper))
    val other = Player("O", hand = EmptyDeck, deck = EmptyDeck, discarded = Deck(Province))
    val game = emptyGame.copy(players = Map(subject.name -> subject, other.name -> other))

    val (_, gameOne) = subject.plays(Spy)(game)

    gameOne.find(other).deck.head shouldBe Province
    gameOne.find(other).discarded shouldBe 'empty
  }

  "Witch" should "translate to: +2 and +1 curse to all the other players" in {
    val subject = Player("P", hand = Deck(Witch), deck = Deck(Copper, Copper))
    val other = Player("O", hand = EmptyDeck, deck = EmptyDeck)
    val another = Player("A", hand = EmptyDeck, deck = EmptyDeck)
    val game = emptyGame.copy(players = Map(subject.name -> subject, other.name -> other, another.name -> another), supplyPiles = Map(Curse -> 2))

    val (stateOne, gameOne) = subject.plays(Witch)(game)

    stateOne.hand.size shouldBe 2
    stateOne.deck shouldBe 'empty

    gameOne.find(other).deck should contain only Curse
    gameOne.find(another).deck should contain only Curse
  }
}
