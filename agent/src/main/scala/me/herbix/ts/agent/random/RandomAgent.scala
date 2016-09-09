package me.herbix.ts.agent.random

import me.herbix.ts.agent.Agent
import me.herbix.ts.logic._

import scala.util.Random

/**
  * Created by Chaofan on 2016/9/9.
  */
class RandomAgent(game: Game, operationCallback: Operation => Unit) extends Agent(game, operationCallback) {

  val rand = new Random()

  override def update(game: Game, hint: OperationHint): Operation = {
    val playerId = game.playerId
    val faction = game.playerFaction
    hint match {
      case OperationHint.CHOOSE_FACTION =>
        new OperationChooseFaction(playerId, if(rand.nextBoolean()) Faction.US else Faction.USSR)

      case OperationHint.SELECT_REGION =>
        new OperationSelectRegion(playerId, faction, Region(rand.nextInt(6)))

      case h: OperationModifyInfluenceHint =>
        var detail = Map.empty[Country, Int]
        var validCountries = h.validCountries(game, detail)

        while (validCountries.nonEmpty) {
          val country = validCountries.take(rand.nextInt(validCountries.size) + 1).last
          if (detail.contains(country)) {
            detail += country -> (detail(country) + 1)
          } else {
            detail += country -> 1
          }
          validCountries = h.validCountries(game, detail)
        }

        new OperationModifyInfluence(playerId, faction, h.isAdd, detail)

      case h: OperationSelectCardHint => null
        //val validCards = h.validCards(game)

        //new OperationSelectCard(playerId, faction, validCards)

      case h: OperationSelectCardsHint => null
      case h: OperationSelectCardAndActionHint => null
      case h: OperationSelectOperationHint => null
      case h: OperationSelectCountryHint => null
      case h: OperationYesNoHint => null
      case h: OperationIntValueHint => null
      case _ => null
    }
  }

}
