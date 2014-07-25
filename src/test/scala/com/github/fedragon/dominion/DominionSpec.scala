package com.github.fedragon.dominion

class DominionSpec extends UnitSpec {

  import Dominion._


  "Drawing cards from a deck" should "reduce the number of remaining cards" in {
    val subject = Deck()
    subject.cards.size shouldBe 84
    val (_, deck) = subject.draw.get

    deck.cards.size shouldBe 83
  }

  "A player" should "be able to buy a card if he has enough coins" in {
    val subject = Player("Player", Vector.fill(7)(Copper), Vector.empty, Deck(Vector.empty))
    subject.canBuy(Estate) shouldBe true

    val poor = Player("Poor", Vector.fill(1)(Copper), Vector.empty, Deck(Vector.empty))
    poor.canBuy(Estate) shouldBe false
  }

  it should "be able to draw from his deck" in {
    val first = new Player("Player", Deck(Vector(Copper, Estate))).draw
    first.hand should contain (Copper)
    first.deck.cards should contain only Estate

    val second = first.draw
    second.deck.cards shouldBe 'empty
    second.hand should contain only (Copper, Estate)
  }

  it should "be able to discard from his hand" in {
    val first = new Player("Player", Deck(Vector(Copper, Estate))).draw
    first.hand should contain(Copper)

    val second = first.discard
    second.hand shouldBe 'empty
    second.discarded should contain(Copper)
  }
}
