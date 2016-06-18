package me.herbix.ts.logic

import me.herbix.ts.logic.Faction.Faction

/**
  * Created by Chaofan on 2016/6/18.
  */
abstract class OperationInput(val playerId: Int)

class OperationInputChooseFaction(playerId: Int, val faction: Faction) extends OperationInput(playerId)
