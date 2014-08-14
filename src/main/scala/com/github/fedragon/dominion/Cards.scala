package com.github.fedragon.dominion

object ActionCards {

  case object Adventurer extends Action("Adventurer", cost = Coins(6)) with Kingdom

  case object Bureaucrat extends Action("Bureaucrat", cost = Coins(4)) with Kingdom

  case object Cellar extends Action("Cellar", cost = Coins(2)) with Kingdom

  case object Chancellor extends Action("Chancellor", cost = Coins(3)) with Kingdom

  case object Chapel extends Action("Chapel", cost = Coins(2)) with Kingdom

  case object CouncilRoom extends Action("Council Room", cost = Coins(5)) with Kingdom

  case object Feast extends Action("Feast", cost = Coins(4)) with Kingdom

  case object Festival extends Action("Festival", cost = Coins(5)) with Kingdom

  case object Laboratory extends Action("Laboratory", cost = Coins(5)) with Kingdom

  case object Library extends Action("Library", cost = Coins(5)) with Kingdom

  case object Market extends Action("Market", cost = Coins(5)) with Kingdom

  case object Militia extends Action("Militia", cost = Coins(4)) with Attack with Kingdom

  case object Mine extends Action("Mine", cost = Coins(5)) with Kingdom

  case object Moat extends Action("Moat", cost = Coins(2)) with Reaction with Kingdom

  case object Moneylender extends Action("Moneylender", cost = Coins(4)) with Kingdom

  case object Remodel extends Action("Remodel", cost = Coins(4)) with Kingdom

  case object Smithy extends Action("Smithy", cost = Coins(4)) with Kingdom

  case object Spy extends Action("Spy", cost = Coins(4)) with Attack with Kingdom

  case object Thief extends Action("Thief", cost = Coins(4)) with Attack with Kingdom

  case object ThroneRoom extends Action("Throne Room", cost = Coins(4)) with Kingdom

  case object Village extends Action("Village", cost = Coins(3)) with Kingdom

  case object Witch extends Action("Witch", cost = Coins(5)) with Attack with Kingdom

  case object Woodcutter extends Action("Woodcutter", cost = Coins(3)) with Kingdom

  case object Workshop extends Action("Workshop", cost = Coins(3)) with Kingdom

}

object TreasureCards {

  case object Copper extends Treasure("Copper", cost = Coins(0), value = Coins(1))

  case object Silver extends Treasure("Silver", cost = Coins(3), value = Coins(2))

  case object Gold extends Treasure("Gold", cost = Coins(6), value = Coins(3))

}

object VictoryCards {

  case object Estate extends Victory("Estate", cost = Coins(2), value = FixedValue(1))

  case object Duchy extends Victory("Duchy", cost = Coins(5), value = FixedValue(3))

  case object Province extends Victory("Province", cost = Coins(8), value = FixedValue(6))

  case object Gardens extends Victory("Gardens", cost = Coins(4), value = FunctionValue(n => math.floor(n / 10).toInt)) with Kingdom

}

