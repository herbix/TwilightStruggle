package me.herbix.ts.util

import me.herbix.ts.logic.Country
import me.herbix.ts.logic.Faction._

/**
  * Created by Chaofan on 2016/9/29.
  */
trait InfluenceProvider {

  def influence(country: Country, faction: Faction): Int

  def getController(country: Country) = {
    val influenceUS = influence(country, US)
    val influenceUSSR = influence(country, USSR)
    if (influenceUS - influenceUSSR >= country.stability)
      US
    else if (influenceUSSR - influenceUS >= country.stability)
      USSR
    else
      Neutral
  }

  def getInfluenceDiff(country: Country, faction: Faction): Int = {
    influence(country, faction) - influence(country, getOpposite(faction))
  }

}
