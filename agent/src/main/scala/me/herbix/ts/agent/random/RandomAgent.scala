package me.herbix.ts.agent.random

import me.herbix.ts.agent.Agent
import me.herbix.ts.logic._

import scala.util.Random

/**
  * Created by Chaofan on 2016/9/9.
  */
class RandomAgent(game: Game, operationCallback: Operation => Unit) extends Agent(game, operationCallback) {

  lazy val rand = new Random()

  override def update(game: Game, hint: OperationHint): Operation = {
    val playerId = game.playerId
    val faction = game.playerFaction
    hint match {
      case OperationHint.CHOOSE_FACTION =>
        if (game.pendingInput == null) {
          null
        } else {
          val pendingInput = game.pendingInput.asInstanceOf[OperationChooseFaction]
          new OperationChooseFaction(playerId, Faction.getOpposite(pendingInput.faction))
        }

      case OperationHint.SELECT_REGION =>
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
