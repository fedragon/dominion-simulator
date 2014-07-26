package com.github.fedragon.dominion

object KingdomCards {

  case object Cellar extends Action("Cellar", cost = Coins(2))

  case object Market extends Action("Market", cost = Coins(5))

  case object Smithy extends Action("Smithy", cost = Coins(4))

}

object TreasureCards {

  case object Copper extends Treasure("Copper", cost = Coins(0), value = CardValue(1))

}

object VictoryCards {

  case object Estate extends Victory("Estate", cost = Coins(2), value = CardValue(1))

}

