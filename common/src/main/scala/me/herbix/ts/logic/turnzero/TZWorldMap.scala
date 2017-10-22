// Copyright (C) 2017 Chaofan

package me.herbix.ts.logic.turnzero

import me.herbix.ts.logic.{Region, Country, WorldMapTrait}

/**
  * Created by Chaofan on 2017/4/4.
  */
object TZWorldMap extends WorldMapTrait {

  val normalTaiwan = countries("Taiwan")
  val battlefieldTaiwan = new Country(normalTaiwan.id, "Taiwan", 3, true, Set(Region.Asia))

  def replaceWithBattlefieldTaiwan(): Unit = {
    removeCountry(normalTaiwan)
    addCountry(battlefieldTaiwan, Set("Japan", "S.Korea"))
  }

  override def reset(): Unit = {
    super.reset()
    if (countries("Taiwan") eq battlefieldTaiwan) {
      removeCountry(battlefieldTaiwan)
      addCountry(normalTaiwan, Set("Japan", "S.Korea"))
    }
  }
}
