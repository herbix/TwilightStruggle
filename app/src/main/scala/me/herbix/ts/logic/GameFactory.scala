package me.herbix.ts.logic

import me.herbix.ts.logic.GameVariant.GameVariant

/**
  * Created by Chaofan on 2016/8/8.
  */
object GameFactory {

  def createGameByVariant(variant: GameVariant): Game = {
    variant match {
      case GameVariant.Standard => new Game()
      case GameVariant.ChineseCivilWar => new GameChineseCivilWar()
      case GameVariant.LateWar => new GameLateWar()
      case _ => null
    }
  }

}
