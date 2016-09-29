package me.herbix.ts.agent.random

import me.herbix.ts.agent.{Agent, AgentBase}
import me.herbix.ts.agent.simulator.GameSimulator
import me.herbix.ts.logic._
import me.herbix.ts.util._

import scala.util.Random

/**
  * Created by Chaofan on 2016/9/9.
  */
class RandomAgent(game: Game, operationCallback: (OperationHint, Operation) => Unit) extends Agent(game, operationCallback) {

  lazy val rand = new Random()
  lazy val simulator = new GameSimulator

  override def pickOperation(game: Game, hint: OperationHint): Operation = {
    val faction = game.playerFaction

    var tryCount = 100

    do {
      simulator.begin(game)
      val gameState = simulator.state
      val candidate = pickCandidateOperation(gameState, hint)

      if (game.operatingPlayer == Faction.Neutral) {
        return candidate
      }

      simulator.extendState(candidate)

      var deadEnd = false
      var searchEnd = false
      do {
        val newState = simulator.state
        if (newState.stateStack.top == State.end) {
          searchEnd = true
          deadEnd = newState.operatingPlayer == Faction.getOpposite(faction)
        } else if (newState.hand(faction).count(!_.canHeld(newState)) > newState.turnRoundCount - newState.round) {
          searchEnd = true
          deadEnd = true
        } else if (newState.getOperationHint == OperationHint.NOP) {
          searchEnd = true
        } else {
          simulator.extendState(pickCandidateOperation(newState, newState.getOperationHint))
        }
      } while (!searchEnd)

      if (!deadEnd) {
        return candidate
      }

      tryCount -= 1
    } while (tryCount > 0)

    pickCandidateOperation(game, hint)
  }

  def pickCandidateOperation(game: Game, hint: OperationHint): Operation = {
    val playerId = game.playerId
    val faction = game.playerFaction

    hint match {
      case h: OperationSelectRegionHint =>
        new OperationSelectRegion(playerId, faction, Region(rand.nextInt(6)))

      case h: OperationModifyInfluenceHint =>
        var detail = Map.empty[Country, Int]
        var validCountries = h.validCountries(game, detail).toSeq

        while (validCountries.nonEmpty) {
          val country = validCountries(rand.nextInt(validCountries.size))
          if (detail.contains(country)) {
            detail += country -> (detail(country) + 1)
          } else {
            detail += country -> 1
          }
          validCountries = h.validCountries(game, detail).toSeq
        }

        new OperationModifyInfluence(playerId, faction, h.isAdd, detail)

      case h: OperationSelectCardHint =>
        val validCards = h.validCards(game).toSeq
        if (validCards.nonEmpty) {
          new OperationSelectCard(playerId, faction, Some(validCards(rand.nextInt(validCards.size))))
        } else {
          new OperationSelectCard(playerId, faction, None)
        }

      case h: OperationSelectCardsHint =>
        val validCards = h.validCards(game).toSeq
        new OperationSelectCards(playerId, faction, validCards.filter(_ => rand.nextBoolean()).toSet)

      case h: OperationSelectCardAndActionHint =>
        val validCards = h.validCards(game).toSeq
        val card = validCards(rand.nextInt(validCards.size))
        val validActions = h.validCardActions(game, card).toSeq

        new OperationSelectCardAndAction(playerId, faction, card, validActions(rand.nextInt(validActions.size)))

      case h: OperationSelectOperationHint =>
        val validOperations = h.validOperations(game).toSeq
        new OperationSelectOperation(playerId, faction, validOperations(rand.nextInt(validOperations.size)))

      case h: OperationSelectCountryHint =>
        var detail = Set.empty[Country]
        var validCountries = h.validCountries(game, detail).toSeq

        while (validCountries.nonEmpty) {
          val country = validCountries(rand.nextInt(validCountries.size))
          detail += country
          validCountries = h.validCountries(game, detail).toSeq
        }

        new OperationSelectCountry(playerId, faction, detail)

      case h: OperationYesNoHint =>
        if (h.isConfirm) {
          new OperationYesNo(playerId, faction, true)
        } else {
          new OperationYesNo(playerId, faction, rand.nextBoolean())
        }

      case h: OperationIntValueHint =>
        new OperationIntValue(playerId, faction, rand.nextInt(h.max - h.min + 1) + h.min)

      case _ => null
    }
  }
}
