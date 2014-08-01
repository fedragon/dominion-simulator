package com.github.fedragon.dominion

object KingdomCards {

  case object Bureaucrat extends Action("Bureaucrat", cost = Coins(4))

  case object Cellar extends Action("Cellar", cost = Coins(2))

  case object CouncilRoom extends Action("CouncilRoom", cost = Coins(5))

  case object Market extends Action("Market", cost = Coins(5))

  case object Militia extends Action("Militia", cost = Coins(4)) with Attack

  case object Mine extends Action("Mine", cost = Coins(5))

  case object Moat extends Action("Moat", cost = Coins(2)) with Reaction

  case object Smithy extends Action("Smithy", cost = Coins(4))

  case object Spy extends Action("Spy", cost = Coins(4)) with Attack

  case object Thief extends Action("Thief", cost = Coins(4)) with Attack

  case object ThroneRoom extends Action("ThroneRoom", cost = Coins(4))

  case object Witch extends Action("Witch", cost = Coins(5)) with Attack

}

object TreasureCards {

  case object Copper extends Treasure("Copper", cost = Coins(0), value = Coins(1))

  case object Silver extends Treasure("Silver", cost = Coins(3), value = Coins(2))

  case object Gold extends Treasure("Gold", cost = Coins(6), value = Coins(3))

}

object VictoryCards {

  case object Curse extends FixedVictory("Curse", cost = Coins(0), value = FixedValue(-1))

  case object Estate extends FixedVictory("Estate", cost = Coins(2), value = FixedValue(1))

  case object Duchy extends FixedVictory("Duchy", cost = Coins(5), value = FixedValue(3))

  case object Province extends FixedVictory("Province", cost = Coins(8), value = FixedValue(6))

  case object Gardens extends FunctionVictory("Gardens", cost = Coins(4), value = FunctionValue(n => math.floor(n / 10).toInt))

}

