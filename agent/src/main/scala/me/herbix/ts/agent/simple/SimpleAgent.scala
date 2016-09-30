package me.herbix.ts.agent.simple

import me.herbix.ts.agent.random.RandomAgent
import me.herbix.ts.logic.Faction._
import me.herbix.ts.logic.Region.RegionState.RegionState
import me.herbix.ts.logic.Region.{Region, RegionState}
import me.herbix.ts.logic._
import me.herbix.ts.util.{OperationModifyInfluenceHint, OperationHint}

/**
  * Created by Chaofan on 2016/9/27.
  */
class SimpleAgent(game: Game, operationCallback: (OperationHint, Operation) => Unit) extends RandomAgent(game, operationCallback) {

  var regionStates = Map.empty[Region, RegionState]

  var countryImportance = Map.empty[Country, Float]
  var countryThreat = Map.empty[Country, Float]

  override def pickOperation(hint: OperationHint): Operation = {
    val playerId = game.playerId
    val faction = game.playerFaction
    val opponentFaction = Faction.getOpposite(faction)

    calculateImportance()
    calculateThreat()

    hint match {
      case h: OperationModifyInfluenceHint =>
        var detail = Map.empty[Country, Int]
        var continue = false
        do {
          val valid = h.validCountries(game, detail)
          continue = valid.nonEmpty

          if (continue) {
            val country = if (h.isAdd)
              valid.maxBy(c => {
                if (getController(game, c, faction, detail) == faction)
                  countryThreat(c) * 0.4
                else if (getController(game, c, faction, detail) == opponentFaction)
                  countryImportance(c) / 4
                else
                  countryImportance(c) - countryThreat(c) * 0.6
              })
            else
              valid.maxBy(c => countryImportance(c))

            if (detail.contains(country)) {
              detail += country -> (detail(country) + 1)
            } else {
              detail += country -> 1
            }
          }
        } while (continue)

        new OperationModifyInfluence(playerId, faction, h.isAdd, detail)

      case _ => super.pickOperation(hint)
    }
  }

  def getController(game: Game, country: Country, faction: Faction, detail: Map[Country, Int]) = {
    val influenceUS = game.countryInfluence(country)(US) + (if (faction == US) detail.getOrElse(country, 0) else 0)
    val influenceUSSR = game.countryInfluence(country)(USSR) + (if (faction == USSR) detail.getOrElse(country, 0) else 0)
    if (influenceUS - influenceUSSR >= country.stability)
      US
    else if (influenceUSSR - influenceUS >= country.stability)
      USSR
    else
      Neutral
  }

  def calculateImportance(): Unit = {
    val faction = game.playerFaction
    val opponentFaction = Faction.getOpposite(faction)

    regionStates = Region.MainRegionSet.map(r => r -> Region.getRegionState(game, r)(faction)).toMap

    countryImportance = WorldMap.normalCountries.values.map(country =>
      country -> calculateImportanceForCountry(faction, opponentFaction, country)
    ).toMap
  }

  def calculateImportanceForCountry(faction: Faction, opponentFaction: Faction, country: Country): Float = {
    val region = country.regions.filter(regionStates.contains).head
    val regionState = regionStates(region)
    val info = Region.ScoringInfo(region)

    val baseImportant: Float = regionState match {
      case RegionState.Nop =>
        info._1
      case RegionState.Presence =>
        val battlefieldDiv =
          WorldMap.regionCountries(region).count(c => c.isBattlefield && game.getController(c) == opponentFaction) -
            WorldMap.regionCountries(region).count(c => c.isBattlefield && game.getController(c) == faction)
        val allDiv =
          WorldMap.regionCountries(region).count(game.getController(_) == opponentFaction) -
            WorldMap.regionCountries(region).count(game.getController(_) == faction)
        val div = Math.max(battlefieldDiv, allDiv) + 1
        if (battlefieldDiv < 0 && allDiv < 0) {
          if (country.isBattlefield) 0 else info._2 - info._1
        } else if (allDiv <= 0) {
          if (!country.isBattlefield) 0 else info._2 - info._1
        } else {
          (info._2 - info._1) * (1f / div * 2 + 0.5f)
        }
      case RegionState.Domination =>
        if (country.isBattlefield) 1 else 0
      case RegionState.Control =>
        0
    }

    val battlefieldImportance = if (country.isBattlefield) 1 else 0

    val opponentNearImportance =
      if ((country.adjacentCountries.contains(WorldMap.countryUS) && faction == Faction.USSR) ||
        (country.adjacentCountries.contains(WorldMap.countryUSSR) && faction == Faction.US))
        1
      else
        0

    baseImportant + battlefieldImportance + opponentNearImportance
  }

  def calculateThreat(): Unit = {
    val faction = game.playerFaction
    val opponentFaction = Faction.getOpposite(faction)
    val cardOp = 4f

    countryThreat = WorldMap.normalCountries.values.map(country =>
      country -> calculateThreatForCountry(cardOp, faction, opponentFaction, country)
    ).toMap
  }

  def calculateThreatForCountry(cardOp: Float, faction: Faction, opponentFaction: Faction, country: Country): Float = {
    val threatInfluence =
      if (country.adjacentCountries.exists(game.influence(_, opponentFaction) > 0) ||
        game.influence(country, opponentFaction) > 0)
        if (game.getController(country) == faction)
          cardOp - (game.influence(country, faction) + game.influence(country, opponentFaction)) / 2f
        else
          cardOp
      else
        0

    val threatRealignment =
      if (game.canRealignment(opponentFaction, country))
        Math.min(
          0.5f * cardOp * (country.adjacentCountries.count(game.getController(_) == opponentFaction) +
            (if (game.influence(country, faction) < game.influence(country, opponentFaction)) 1 else 0)),
          game.influence(country, faction)
        )
      else
        0

    val threatCoup =
      if (game.canCoup(opponentFaction, country) &&
        (!country.isBattlefield || game.defcon > 2 || game.flags.hasFlag(opponentFaction, Flags.NuclearSubs)))
        3.5f + cardOp - country.stability * 2
      else
        0

    Math.max(threatInfluence, Math.max(threatRealignment, threatCoup))
  }
}
