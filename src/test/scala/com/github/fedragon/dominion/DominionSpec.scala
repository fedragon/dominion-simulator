package com.github.fedragon.dominion

class DominionSpec extends UnitSpec {

  import Dominion._

  val subject = Deck()

  "A deck" should "allow to draw cards" in {
    subject.cards shouldBe Map[Card, Int](Copper -> 60, Silver -> 40, Gold -> 30)

    val updated = subject -= (Copper, 3) -= (Silver, 6) -= (Gold, 13)
    updated.cards shouldBe Map[Card, Int](Copper -> 57, Silver -> 34, Gold -> 17)
  }
}
