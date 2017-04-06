package me.herbix.ts.logic.turnzero

import me.herbix.ts.logic.{GameVariant, GameRecordingHistory}

/**
  * Created by Chaofan on 2017/4/3.
  */
class GameTurnZero extends GameRecordingHistory {

  lazy override val gameVariant = GameVariant.TurnZero

  lazy override val theCards = TZCards
  lazy override val theWorldMap = TZWorldMap

}
