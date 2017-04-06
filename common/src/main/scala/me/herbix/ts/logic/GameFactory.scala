package me.herbix.ts.logic

import me.herbix.ts.logic.GameVariant.GameVariant
import me.herbix.ts.logic.chinesecivilwar.GameChineseCivilWar
import me.herbix.ts.logic.latewar.GameLateWar
import me.herbix.ts.logic.turnzero.GameTurnZero

/**
  * Created by Chaofan on 2016/8/8.
  */
object GameFactory {

  def createGameByVariant(variant: GameVariant): Game = {
    variant match {
      case GameVariant.ChineseCivilWar =>
        new GameChineseCivilWar
      case GameVariant.Standard =>
        new GameRecordingHistory
      case GameVariant.LateWar =>
        new GameLateWar
      case GameVariant.TurnZero =>
        new GameTurnZero
      case _ =>
        null
    }
  }

}
