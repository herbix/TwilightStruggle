package me.herbix.ts.logic

import me.herbix.ts.logic.Action.Action
import me.herbix.ts.logic.Faction.Faction

/**
  * Created by Chaofan on 2016/6/18.
  */
abstract class Operation(val playerId: Int)

class OperationChooseFaction(playerId: Int, val faction: Faction) extends Operation(playerId)

class OperationModifyInfluence(playerId: Int, val faction: Faction, val isAdd: Boolean, val detail: Map[Country, Int])
  extends Operation(playerId)

class OperationSelectCard(playerId: Int, val faction: Faction, val card: Card) extends Operation(playerId)

class OperationSelectCardAndAction(playerId: Int, faction: Faction, card: Card, val action: Action)
  extends OperationSelectCard(playerId, faction, card)
