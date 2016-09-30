package me.herbix.ts.agent.simple

import me.herbix.ts.agent.random.RandomAgent
import me.herbix.ts.logic.Faction._
import me.herbix.ts.logic.Region.RegionState.RegionState
import me.herbix.ts.logic.Region.{Region, RegionState}
import me.herbix.ts.logic._
import me.herbix.ts.logic.card.Cards
import me.herbix.ts.util._

/**
  * Created by Chaofan on 2016/9/27.
  */
class SimpleAgent(game: Game, operationCallback: (OperationHint, Operation) => Unit) extends RandomAgent(game, operationCallback) {

  var playerId = 0
  var agentFaction = Neutral
  var opponentFaction = Neutral

  var regionStates = Map.empty[Region, RegionState]

  var countryState = Map.empty[Country, CountryState]

  override def pickOperation(hint: OperationHint): Operation = {
    playerId = game.playerId
    agentFaction = game.playerFaction
    opponentFaction = Faction.getOpposite(agentFaction)

    updateCountryState(game)

    hint match {
      case h: OperationModifyInfluenceHint =>
        var influenceProvider: InfluenceProvider = game
        var detail = Map.empty[Country, Int]
        var continue = false
        do {
          val valid = h.validCountries(game, detail)
          continue = valid.nonEmpty

          if (continue) {
            val country = if (h.isAdd)
              valid.maxBy(c => countryState(c).addInfluenceScore)
            else
              valid.maxBy(c => countryState(c).removeInfluenceScore)

            if (detail.contains(country)) {
              detail += country -> (detail(country) + 1)
            } else {
              detail += country -> 1
            }

            influenceProvider = new DetailInfluenceProvider(game, detail, h.isAdd, h.targetFaction)
            updateCountryState(influenceProvider)
          }
        } while (continue)

        new OperationModifyInfluence(playerId, agentFaction, h.isAdd, detail)

      case h: OperationSelectCardAndActionHint =>
        val scoringCardCount = game.hand(agentFaction).count(!_.canHeld(game))
        if (scoringCardCount > 0 && game.turn >= game.turnRoundCount - scoringCardCount + 1) {
          val scoringCards = game.hand(agentFaction).filter(!_.canHeld(game)).toSeq
          new OperationSelectCardAndAction(playerId, agentFaction, scoringCards.head, Action.Event)
        } else {
          val cards = h.validCards(game).filter(c => c != Cards.chinaCard && h.validCardActions(game, c).contains(Action.Operation))
          if (cards.nonEmpty) {
            new OperationSelectCardAndAction(playerId, agentFaction, cards.head, Action.Operation)
          } else {
            super.pickOperation(hint)
          }
        }

      case h: OperationSelectOperationHint =>
        if (h.canInfluence(game)) {
          new OperationSelectOperation(playerId, agentFaction, Action.Influence)
        } else {
          super.pickOperation(hint)
        }

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
      this,
      influenceProvider,
      country,
      importance = calculateImportanceForCountry(influenceProvider, country),
      threat = calculateGainForFactionAndCountry(4, influenceProvider, getOpposite(game.playerFaction), country),
      gain = calculateGainForFactionAndCountry(4, influenceProvider, game.playerFaction, country)
    )
  }

  def calculateImportanceForCountry(influenceProvider: InfluenceProvider, country: Country): Float = {
    val region = country.regions.filter(regionStates.contains).head
    val regionState = regionStates(region)
    val info = Region.ScoringInfo(region)

    val baseImportant: Float = regionState match {
      case RegionState.Nop =>
        info._1
      case RegionState.Presence =>
        val battlefieldDiv =
          WorldMap.regionCountries(region).count(c => c.isBattlefield && influenceProvider.getController(c) == opponentFaction) -
            WorldMap.regionCountries(region).count(c => c.isBattlefield && influenceProvider.getController(c) == agentFaction)
        val allDiv =
          WorldMap.regionCountries(region).count(influenceProvider.getController(_) == opponentFaction) -
            WorldMap.regionCountries(region).count(influenceProvider.getController(_) == agentFaction)
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
      if ((country.adjacentCountries.contains(WorldMap.countryUS) && agentFaction == USSR) ||
        (country.adjacentCountries.contains(WorldMap.countryUSSR) && agentFaction == US))
        1
      else
        0

    baseImportant + battlefieldImportance + opponentNearImportance
  }

  def calculateGainForFactionAndCountry(cardOp: Float, influenceProvider: InfluenceProvider, faction: Faction, country: Country): Float = {
    val opponentFaction = Faction.getOpposite(faction)

    val threatInfluence =
      if ((country.adjacentCountries + country).exists(influenceProvider.influence(_, faction) > 0))
        if (influenceProvider.getController(country) == opponentFaction)
          Math.max(cardOp / 2, cardOp - (influenceProvider.getInfluenceDiff(country, opponentFaction) - country.stability + 1))
        else
          cardOp
      else
        0

    val threatRealignment =
      if (game.canRealignment(faction, country))
        Math.min(
          0.5f * cardOp * (country.adjacentCountries.count(influenceProvider.getController(_) == faction) +
            (if (influenceProvider.influence(country, opponentFaction) < influenceProvider.influence(country, faction)) 1 else 0)),
          influenceProvider.influence(country, opponentFaction)
        )
      else
        0

    val threatCoup =
      if (game.canCoup(faction, country) &&
        (!country.isBattlefield || game.defcon > 2 || game.flags.hasFlag(faction, Flags.NuclearSubs)))
        3.5f + cardOp - country.stability * 2
      else
        0

    Seq(threatInfluence, threatRealignment, threatCoup).max
  }
}
