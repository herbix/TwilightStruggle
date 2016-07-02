package me.herbix.ts.logic

import me.herbix.ts.logic.Action.Action
import me.herbix.ts.logic.Faction.Faction
import me.herbix.ts.logic.Region.Region

/**
  * Created by Chaofan on 2016/6/18.
  */
abstract class Operation(val playerId: Int, val faction: Faction)

class OperationChooseFaction(playerId: Int, faction: Faction) extends Operation(playerId, faction)

class OperationModifyInfluence(playerId: Int, faction: Faction, val isAdd: Boolean, val detail: Map[Country, Int])
  extends Operation(playerId, faction)

class OperationSelectCard(playerId: Int, faction: Faction, val card: Card) extends Operation(playerId, faction)

class OperationSelectCardAndAction(playerId: Int, faction: Faction, card: Card, val action: Action)
  extends OperationSelectCard(playerId, faction, card)

class OperationSelectOperation(playerId: Int, faction: Faction, val action: Action) extends Operation(playerId, faction)

class OperationSelectCountry(playerId: Int, faction: Faction, val detail: Set[Country]) extends Operation(playerId, faction)

class OperationYesNo(playerId: Int, faction: Faction, val value: Boolean) extends Operation(playerId, faction)

class OperationIntValue(playerId: Int, faction: Faction, val value: Int) extends Operation(playerId, faction)

class OperationSelectRegion(playerId: Int, faction: Faction, val region: Region) extends Operation(playerId, faction)
