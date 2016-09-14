package me.herbix.ts.logic

import me.herbix.ts.logic.GameVariant.GameVariant

/**
  * Created by Chaofan on 2016/8/8.
  */
object GameFactory {

  def createGameByVariant(variant: GameVariant): Game = {
    val result = new GameRecordingHistory
    result.gameVariant = variant
    result
  }

}
