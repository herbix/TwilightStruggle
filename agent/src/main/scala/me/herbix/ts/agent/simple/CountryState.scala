package me.herbix.ts.agent.simple

import me.herbix.ts.logic.Faction._
import me.herbix.ts.logic.Region.RegionState
import me.herbix.ts.logic._
import me.herbix.ts.util.InfluenceProvider

/**
  * Created by Chaofan on 2016/9/30.
  */
class CountryState(game: Game,
                   agent: SimpleAgent,
                   influenceProvider: InfluenceProvider,
                   selfCardOp: Int,
                   opponentMaxCardOp: Float,
                   val country: Country) {

  val self = agent.agentFaction
  val opponent = agent.opponentFaction

  val regionState = agent.regionStates(country.regions.find(Region.MainRegionSet).get)

  val controlDiff = country.stability.toFloat - influenceProvider.getInfluenceDiff(country, self)

  val opponentInfluenceByAdd = calculateInfluenceByAdd(opponentMaxCardOp, opponent)
  val opponentInfluenceByAddIfControl = calculateInfluenceByAddIfControl(opponentMaxCardOp, opponent)
  val opponentInfluenceByRealignment = calculateInfluenceByRealignment(opponentMaxCardOp, opponent)
  val opponentInfluenceByCoup = calculateInfluenceByCoup(opponentMaxCardOp, opponent)

  val opponentInfluence = Set(opponentInfluenceByAdd, opponentInfluenceByRealignment, opponentInfluenceByCoup).max
  
  val selfInfluenceByAdd = calculateInfluenceByAdd(selfCardOp, self)
  val selfInfluenceByAddIfControl = calculateInfluenceByAddIfControl(selfCardOp, self)
  val selfInfluenceByRealignment = calculateInfluenceByRealignment(opponentMaxCardOp, self)
  val selfInfluenceByCoup = calculateInfluenceByCoup(opponentMaxCardOp, self)

  val selfInfluence = Set(selfInfluenceByAdd, selfInfluenceByRealignment, selfInfluenceByCoup).max

  val selfImportance = calculateImportance(self)
  val opponentImportance = calculateImportance(opponent)

  override def toString = f"$selfImportance%.1f:$opponentImportance%.1f"

  private def calculateInfluenceByAdd(cardOp: Float, faction: Faction): Float = {
    if ((country.adjacentCountries + country).exists(influenceProvider.influence(_, faction) > 0))
      if (influenceProvider.getController(country) == Faction.getOpposite(faction))
        calculateInfluenceByAddIfControl(cardOp, faction)
      else
        cardOp
    else
      0
  }

  private def calculateInfluenceByAddIfControl(cardOp: Float, faction: Faction): Float = {
    Math.max(cardOp / 2, cardOp - (influenceProvider.getInfluenceDiff(country, Faction.getOpposite(faction)) - country.stability + 1))
  }

  private def calculateInfluenceByRealignment(cardOp: Float, faction: Faction): Float = {
    val opponentFaction = Faction.getOpposite(faction)
    if (game.canRealignment(faction, country)) {
      val influenceFactor =
        if (influenceProvider.getInfluenceDiff(country, opponentFaction) < 0)
          1
        else if (influenceProvider.getInfluenceDiff(country, opponentFaction) > 0)
          -1
        else
          0
      val realignmentCount = 0.5f * cardOp * (
        country.adjacentCountries.count(influenceProvider.getController(_) == faction) +
          -country.adjacentCountries.count(influenceProvider.getController(_) == opponentFaction) +
          influenceFactor
        )
      Math.max(
        Math.min(realignmentCount, influenceProvider.influence(country, opponentFaction)),
        -influenceProvider.influence(country, faction)
      )
    } else
      0
  }

  private def calculateInfluenceByCoup(cardOp: Float, faction: Faction): Float = {
    if (game.canCoup(faction, country) &&
      (!country.isBattlefield || game.defcon > 2 || game.flags.hasFlag(faction, Flags.NuclearSubs)))
      3.5f + cardOp - country.stability * 2
    else
      0
  }

  private def calculateImportance(faction: Faction): Float = {
    if (influenceProvider.getController(country) == faction) {
      return 0
    }

    var importance = calculateBaseImportance(faction)

    if (country.isBattlefield) importance += 1
    if ((country.adjacentCountries.contains(game.theWorldMap.countryUS) && faction == USSR) ||
      (country.adjacentCountries.contains(game.theWorldMap.countryUSSR) && faction == US)) {
      importance += 1
    }

    importance
  }

  private def calculateBaseImportance(faction: Faction): Float = {

    val opponentFaction = Faction.getOpposite(faction)

    regionState.state(faction) match {
      case RegionState.Nop =>
        regionState.nopGain.toFloat / country.stability
      case RegionState.Presence =>
        calculatePresenceImportance(faction, opponentFaction)
      case RegionState.Domination =>
        calculateDominationImportance(faction, opponentFaction)
      case RegionState.Control =>
        0
    }
  }

  private def calculatePresenceImportance(faction: Faction, opponentFaction: Faction): Float = {
    if (influenceProvider.getController(country) == faction) {
      return 0
    }

    val controlDiff = country.stability.toFloat - influenceProvider.getInfluenceDiff(country, faction)

    var selfBattlefield = regionState.battlefieldCountForFaction(faction)
    var opponentBattlefield = regionState.battlefieldCountForFaction(opponentFaction)
    var selfAll = regionState.allCountForFaction(faction)
    var opponentAll = regionState.allCountForFaction(opponentFaction)

    var result = 0f
    var done = false

    val gain = if (regionState.state(opponent) <= RegionState.Presence) {
      regionState.presenceGain
    } else if (regionState.state(opponent) == RegionState.Domination) {
      2 * regionState.presenceGain
    } else {
      regionState.presenceGain + regionState.dominationGain
    }

    if (influenceProvider.getController(country) == opponentFaction) {
      if (selfAll + 1 > opponentAll - 1) {
        if ((country.isBattlefield && (selfBattlefield + 1 > opponentBattlefield - 1)) ||
          (!country.isBattlefield && (selfBattlefield > opponentBattlefield))) {
          result = gain * importanceDecrease(controlDiff)
          done = true
        }
      }

      if (!done) {
        selfAll += 1
        opponentAll -= 1
        if (country.isBattlefield) {
          selfBattlefield += 1
          opponentBattlefield -= 1
        }
      }
    } else {
      if (selfAll + 1 > opponentAll) {
        if ((country.isBattlefield && (selfBattlefield + 1 > opponentBattlefield)) ||
          (!country.isBattlefield && (selfBattlefield > opponentBattlefield))) {
          result = gain * importanceDecrease(controlDiff)
          done = true
        }
      }

      if (!done) {
        selfAll += 1
        if (country.isBattlefield) {
          selfBattlefield += 1
        }
      }
    }

    if (!done) {
      result = gain * importanceDecrease(
        controlDiff +
        Math.max(opponentBattlefield - selfBattlefield + 1, opponentAll - selfAll + 1) / 1.5f * 3
      )
    }

    result
  }

  private def calculateDominationImportance(faction: Faction, opponentFaction: Faction): Float = {
    if (influenceProvider.getController(country) == faction) {
      return 0
    }

    val controlDiff = country.stability.toFloat - influenceProvider.getInfluenceDiff(country, faction)

    val battlefield = regionState.battlefieldCount
    var selfBattlefield = regionState.battlefieldCountForFaction(faction)
    var opponentBattlefield = regionState.battlefieldCountForFaction(opponentFaction)
    var selfAll = regionState.allCountForFaction(faction)
    var opponentAll = regionState.allCountForFaction(opponentFaction)

    var result = 0f
    var done = false

    val gain = regionState.dominationGain

    if (influenceProvider.getController(country) == opponentFaction) {
      if (selfAll + 1 >= opponentAll - 1) {
        if ((country.isBattlefield && (selfBattlefield + 1 == battlefield)) ||
          (!country.isBattlefield && (selfBattlefield == battlefield))) {
          result = gain * importanceDecrease(controlDiff)
          done = true
        }
      }

      if (!done) {
        selfAll += 1
        opponentAll -= 1
        if (country.isBattlefield) {
          selfBattlefield += 1
          opponentBattlefield -= 1
        }
      }
    } else {
      if (selfAll + 1 >= opponentAll) {
        if ((country.isBattlefield && (selfBattlefield + 1 == battlefield)) ||
          (!country.isBattlefield && (selfBattlefield == battlefield))) {
          result = gain * importanceDecrease(controlDiff)
          done = true
        }
      }

      if (!done) {
        selfAll += 1
        if (country.isBattlefield) {
          selfBattlefield += 1
        }
      }
    }

    if (!done) {
      result = gain * importanceDecrease(
        controlDiff +
          Math.max(battlefield - selfBattlefield, (opponentAll - selfAll + 1) / 1.5f) * 3
      )
    }

    result
  }

  def importanceDecrease(controlDiff: Float): Float = {
    Math.pow(0.9, controlDiff - 1).toFloat
  }
}
