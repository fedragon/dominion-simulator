package com.github.fedragon.dominion

object KingdomCards {

  case object Cellar extends Action("Cellar", cost = Coins(2))

  case object Market extends Action("Market", cost = Coins(5))

  case object Mine extends Action("Mine", cost = Coins(5))

  case object Moat extends Action("Moat", cost = Coins(2)) with Reaction

  case object Smithy extends Action("Smithy", cost = Coins(4))

  case object Witch extends Action("Witch", cost = Coins(5)) with Attack

}

object TreasureCards {

  case object Copper extends Treasure("Copper", cost = Coins(0), value = Coins(1))

  case object Silver extends Treasure("Silver", cost = Coins(3), value = Coins(2))

  case object Gold extends Treasure("Gold", cost = Coins(6), value = Coins(3))

}

object VictoryCards {

  case object Curse extends Victory("Curse", cost = Coins(0), value = CardValue(-1))

  case object Estate extends Victory("Estate", cost = Coins(2), value = CardValue(1))

  case object Duchy extends Victory("Duchy", cost = Coins(5), value = CardValue(3))

  case object Province extends Victory("Province", cost = Coins(8), value = CardValue(6))

}

