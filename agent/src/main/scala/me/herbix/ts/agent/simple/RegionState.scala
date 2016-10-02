package me.herbix.ts.agent.simple

import me.herbix.ts.logic.Region.Region
import me.herbix.ts.logic.{Region, Faction, Game}
import me.herbix.ts.logic.Region.Region
import me.herbix.ts.util.InfluenceProvider

/**
  * Created by Chaofan on 2016/10/2.
  */
class RegionState(game: Game,
                  agent: SimpleAgent,
                  influenceProvider: InfluenceProvider,
                  val region: Region){

  val battlefieldCount = influenceProvider.getBattlefieldCount(region)
  val battlefieldCountForFaction = Faction.values.map(f => f -> influenceProvider.getBattlefieldCountForFaction(region, f)).toMap
  val nonBattlefieldCountForFaction = Faction.values.map(f => f -> influenceProvider.getBattlefieldCountForFaction(region, f)).toMap

  val allCountForFaction = Faction.values.map(f => f -> influenceProvider.getAllCountForFaction(region, f)).toMap

  val state = influenceProvider.getRegionState(region)

  val (nopGain, presenceGain, dominationGain) = {
    val info = Region.ScoringInfo(region)
    val control = if (info._3 > 200) {
      20 - Math.abs(game.vp)
    } else {
      info._3
    }
    (info._1, info._2 - info._1, control - info._2)
  }
}
