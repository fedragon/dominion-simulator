package com.github.fedragon.dominion

class DominionSpec extends UnitSpec {

  "Dominion" should "let players play a game" in {
    Dominion.playGame(Vector("X", "Y", "Z"))
  }
}

