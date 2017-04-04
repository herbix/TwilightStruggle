package me.herbix.ts.agent.simple

import me.herbix.ts.agent.random.RandomAgent
import me.herbix.ts.logic.Faction._
import me.herbix.ts.logic.Region.Region
import me.herbix.ts.logic._
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
        super.pickOperation(hint)

        var influenceProvider: InfluenceProvider = game
        var detail = Map.empty[Country, Int]
        var continue = false
        do {
          val valid = h.validCountries(game, detail)
          continue = valid.nonEmpty

          if (continue) {
            val country = if (h.isAdd)
              valid.maxBy(c => {
                val s = countryState(c)
                import s._
                if (influenceProvider.getController(c) == opponentFaction) {
                  selfImportance * (1 + selfInfluence) / (1 + opponentInfluence)
                } else {
                  selfImportance * (1 + selfInfluence) / (1 + Set(opponentInfluenceByAddIfControl, opponentInfluenceByRealignment, opponentInfluenceByCoup).max)
                }
              })
            else
              valid.maxBy(c => countryState(c).opponentImportance)

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
        if (scoringCardCount > 0 && game.round >= game.turnRoundCount - scoringCardCount + 1) {
          val scoringCards = game.hand(agentFaction).filter(!_.canHeld(game)).toSeq
          new OperationSelectCardAndAction(playerId, agentFaction, scoringCards.head, Action.Event)
        } else {
          val cards = h.validCards(game).filter(c => c != game.theCards.chinaCard && h.validCardActions(game, c).contains(Action.Operation))
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
    regionStates = Region.MainRegionSet.map(region =>
      region -> new RegionState(game, this, influenceProvider, region)
    ).toMap
    countryState = game.theWorldMap.normalCountries.values.map(country =>
      country -> new CountryState(game, this, influenceProvider, 4, 4, country)
    ).toMap
  }

  /*def calculateImportanceForCountry(influenceProvider: InfluenceProvider, country: Country): Float = {
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
  }*/

}
