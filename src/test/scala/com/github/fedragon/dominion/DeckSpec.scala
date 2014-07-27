package com.github.fedragon.dominion

import Deck._
import TreasureCards._

class DeckSpec extends UnitSpec {

  "Drawing from a deck" should "reduce the number of remaining cards" in {
    val deck = Deck(Copper)
    deck.size shouldBe 1
    val (_, newDeck) = deck.draw.get

    newDeck shouldBe 'empty
  }

}

