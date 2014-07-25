package com.github.fedragon.dominion

class DominionSpec extends UnitSpec {

  import Dominion._


  "Drawing cards from a deck" should "reduce the number of remaining cards" in {
    val subject = Deck(Vector(Copper))
    subject.cards.size shouldBe 1
    val (_, deck) = subject.draw.get

    deck.cards shouldBe 'empty
  }

  "A player" should "be able to buy a card if he has enough coins" in {
    val subject = Player("Player", Vector.fill(7)(Copper), Vector.empty, Deck(Vector.empty))
    subject.canBuy(Estate) shouldBe true

    val poor = Player("Poor", Vector.fill(1)(Copper), Vector.empty, Deck(Vector.empty))
    poor.canBuy(Estate) shouldBe false
  }

  it should "be able to draw from his deck" in {
    val first = new Player("Player", deck = Deck(Vector(Copper, Estate))).draw
    first.hand should contain (Copper)
    first.deck.cards should contain only Estate

    val second = first.draw
    second.deck.cards shouldBe 'empty
    second.hand should contain only (Copper, Estate)
  }

  it should "be able to discard from his hand" in {
    val first = new Player("Player", deck = Deck(Vector(Copper, Estate))).draw
    first.hand should contain(Copper)

    val second = first.discard
    second.hand shouldBe 'empty
    second.discarded should contain(Copper)
  }

  it should "play an action, if there is at least one in his hand" in {
    val first = Player("Player", hand = Vector(Smithy), discarded = Vector.empty, deck = Deck(Vector(Copper, Estate, Copper)))
    val second = first.playAction

    second.hand.size shouldBe 4
    second.deck.cards shouldBe 'empty
  }

  it should "not play an action, if there are not any in his hand" in {
    val first = new Player("Player", deck = Deck(Vector(Copper, Estate)))
    first.playAction shouldBe first
  }
}
