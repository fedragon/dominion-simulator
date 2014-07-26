package com.github.fedragon.dominion

import Dominion._

object KingdomCards {
  case object Market extends Action("Market", cost = Coins(5)) {
    def play(p: Player): Player = p.draw.addBonuses(Turn(1, 1, 1)) // Draw 1 card, add 1 action, 1 buy, 1 extra coin
  }
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

