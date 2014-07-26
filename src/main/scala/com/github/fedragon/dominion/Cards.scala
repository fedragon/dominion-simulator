package com.github.fedragon.dominion

object KingdomCards {

  case object Cellar extends Action("Cellar", cost = Coins(2))

  case object Market extends Action("Market", cost = Coins(5))

  case object Mine extends Action("Mine", cost = Coins(5))

  case object Smithy extends Action("Smithy", cost = Coins(4))

  case object Witch extends Action("Witch", cost = Coins(5))

}

object TreasureCards {

  case object Copper extends Treasure("Copper", cost = Coins(0), value = CardValue(1))

  case object Silver extends Treasure("Silver", cost = Coins(3), value = CardValue(2))

}

object VictoryCards {

  case object Curse extends Victory("Curse", cost = Coins(0), value = CardValue(-1))

  case object Estate extends Victory("Estate", cost = Coins(2), value = CardValue(1))

}

