// Copyright (C) 2017 Chaofan

package me.herbix.ts.logic.chinesecivilwar

import me.herbix.ts.logic.{Flag, FlagType, FlagsTrait}

/**
  * Created by Chaofan on 2017/4/3.
  */
object CCWFlags extends FlagsTrait {
  val FlagIdOffset = FlagsTrait.flagId
  val ChineseCivilWar = new Flag(FlagType.Always, false)
}
