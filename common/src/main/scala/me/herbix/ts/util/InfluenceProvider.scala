// Copyright (C) 2017 Chaofan

package me.herbix.ts.util

import me.herbix.ts.logic.Faction._
import me.herbix.ts.logic.Region.Region
import me.herbix.ts.logic.Region.RegionState._
import me.herbix.ts.logic.{WorldMapTrait, Country, WorldMap}

/**
  * Created by Chaofan on 2016/9/29.
  */
trait InfluenceProvider {

  lazy val theWorldMap: WorldMapTrait = WorldMap

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

  def getRegionState(region: Region): Map[Faction, RegionState] = {
    val targetCountries = theWorldMap.regionCountries(region)
    val battlefieldCount = targetCountries.count(_.isBattlefield)
    val usBattlefield = targetCountries.count(country => country.isBattlefield && getController(country) == US)
    val usNonBattlefield = targetCountries.count(country => !country.isBattlefield && getController(country) == US)
    val ussrBattlefield = targetCountries.count(country => country.isBattlefield && getController(country) == USSR)
    val ussrNonBattlefield = targetCountries.count(country => !country.isBattlefield && getController(country) == USSR)
    val usAll = usBattlefield + usNonBattlefield
    val ussrAll = ussrBattlefield + ussrNonBattlefield

    val usPresence = usBattlefield > 0 || usNonBattlefield > 0
    val ussrPresence = ussrBattlefield > 0 || ussrNonBattlefield > 0
    val usDomination = usBattlefield > ussrBattlefield && usAll > ussrAll && usNonBattlefield > 0
    val ussrDomination = ussrBattlefield > usBattlefield && ussrAll > usAll && ussrNonBattlefield > 0
    val usControl = usBattlefield == battlefieldCount && usAll > ussrAll
    val ussrControl = ussrBattlefield == battlefieldCount && ussrAll > usAll

    val usState = if (usControl) Control else if (usDomination) Domination else if (usPresence) Presence else Nop
    val ussrState = if (ussrControl) Control else if (ussrDomination) Domination else if (ussrPresence) Presence else Nop

    Map(US -> usState, USSR -> ussrState)
  }

  def getBattlefieldCount(region: Region): Int = {
    theWorldMap.regionCountries(region).count(_.isBattlefield)
  }

  def getBattlefieldCountForFaction(region: Region, faction: Faction): Int = {
    theWorldMap.regionCountries(region).count(country => country.isBattlefield && getController(country) == faction)
  }

  def getNonBattlefieldCountForFaction(region: Region, faction: Faction): Int = {
    theWorldMap.regionCountries(region).count(country => !country.isBattlefield && getController(country) == faction)
  }

  def getAllCountForFaction(region: Region, faction: Faction): Int = {
    theWorldMap.regionCountries(region).count(getController(_) == faction)
  }

}
