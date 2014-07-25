package com.github.fedragon.dominion

import Dominion._

object KingdomCards {
  case object Smithy extends Action("Smithy", cost = Coins(4)) {
    def play(p: Player): Player = p.draw.draw.draw // Draw 3 cards
  }
}

object TreasureCards {
  case object Copper extends Treasure("Copper", cost = Coins(0), value = CardValue(1))
}

object VictoryCards {
  case object Estate extends Victory("Estate", cost = Coins(2), value = CardValue(1))
}

