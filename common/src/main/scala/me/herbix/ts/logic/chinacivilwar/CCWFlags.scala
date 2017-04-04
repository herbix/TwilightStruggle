package me.herbix.ts.logic.chinacivilwar

import me.herbix.ts.logic.{Flag, FlagType, FlagsTrait}

/**
  * Created by Chaofan on 2017/4/3.
  */
object CCWFlags extends FlagsTrait {
  val ChineseCivilWar = new Flag(FlagType.Always, false)
}
