package me.herbix.ts.agent.simple

import me.herbix.ts.agent.random.RandomAgent
import me.herbix.ts.logic.Faction._
import me.herbix.ts.logic.Region.RegionState.RegionState
import me.herbix.ts.logic.Region.{Region, RegionState}
import me.herbix.ts.logic._
import me.herbix.ts.util.{InfluenceProvider, OperationModifyInfluenceHint, OperationHint}

/**
  * Created by Chaofan on 2016/9/27.
  */
class SimpleAgent(game: Game, operationCallback: (OperationHint, Operation) => Unit) extends RandomAgent(game, operationCallback) {

  var regionStates = Map.empty[Region, RegionState]

  var countryState = Map.empty[Country, CountryState]

  override def pickOperation(hint: OperationHint): Operation = {
    val playerId = game.playerId
    val faction = game.playerFaction
    val opponentFaction = Faction.getOpposite(faction)

    var influenceProvider: InfluenceProvider = game

    updateCountryState(influenceProvider)

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
                val s = countryState(c)
                if (influenceProvider.getController(c) == faction)
                  s.threat * 0.4
                else if (influenceProvider.getController(c) == opponentFaction)
                  s.importance / 4
                else
                  s.importance - s.threat * 0.6
              })
            else
              valid.maxBy(c => countryState(c).importance)

            if (detail.contains(country)) {
              detail += country -> (detail(country) + 1)
            } else {
              detail += country -> 1
            }

            influenceProvider = new DetailInfluenceProvider(game, detail, h.isAdd, h.targetFaction)
            updateCountryState(influenceProvider)
          }
        } while (continue)

        new OperationModifyInfluence(playerId, faction, h.isAdd, detail)

      case _ => super.pickOperation(hint)
    }
  }

  def updateCountryState(influenceProvider: InfluenceProvider): Unit = {
    regionStates = Region.MainRegionSet.map(r => r -> influenceProvider.getRegionState(r)(game.playerFaction)).toMap
    countryState = WorldMap.normalCountries.values.map(country =>
      country -> updateCountryStateForCountry(influenceProvider, country)
    ).toMap
  }

  def updateCountryStateForCountry(influenceProvider: InfluenceProvider, country: Country): CountryState = {
    new CountryState(
      importance = calculateImportanceForCountry(influenceProvider, country),
      threat = calculateThreatForCountry(4, influenceProvider, country)
    )
  }

  def calculateImportanceForCountry(influenceProvider: InfluenceProvider, country: Country): Float = {
    val faction = game.playerFaction
    val opponentFaction = Faction.getOpposite(faction)

    val region = country.regions.filter(regionStates.contains).head
    val regionState = regionStates(region)
    val info = Region.ScoringInfo(region)

    val baseImportant: Float = regionState match {
      case RegionState.Nop =>
        info._1
      case RegionState.Presence =>
        val battlefieldDiv =
          WorldMap.regionCountries(region).count(c => c.isBattlefield && influenceProvider.getController(c) == opponentFaction) -
            WorldMap.regionCountries(region).count(c => c.isBattlefield && influenceProvider.getController(c) == faction)
        val allDiv =
          WorldMap.regionCountries(region).count(influenceProvider.getController(_) == opponentFaction) -
            WorldMap.regionCountries(region).count(influenceProvider.getController(_) == faction)
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

  def calculateThreatForCountry(cardOp: Float, influenceProvider: InfluenceProvider, country: Country): Float = {
    val faction = game.playerFaction
    val opponentFaction = Faction.getOpposite(faction)

    val threatInfluence =
      if (country.adjacentCountries.exists(influenceProvider.influence(_, opponentFaction) > 0) ||
        influenceProvider.influence(country, opponentFaction) > 0)
        if (influenceProvider.getController(country) == faction)
          Math.max(cardOp / 2, cardOp - (influenceProvider.getInfluenceDiff(country, faction) - country.stability + 1))
        else
          cardOp
      else
        0

    val threatRealignment =
      if (game.canRealignment(opponentFaction, country))
        Math.min(
          0.5f * cardOp * (country.adjacentCountries.count(influenceProvider.getController(_) == opponentFaction) +
            (if (influenceProvider.influence(country, faction) < influenceProvider.influence(country, opponentFaction)) 1 else 0)),
          influenceProvider.influence(country, faction)
        )
      else
        0

    val threatCoup =
      if (game.canCoup(opponentFaction, country) &&
        (!country.isBattlefield || game.defcon > 2 || game.flags.hasFlag(opponentFaction, Flags.NuclearSubs)))
        3.5f + cardOp - country.stability * 2
      else
        0

    Seq(threatInfluence, threatRealignment, threatCoup).max
  }
}
