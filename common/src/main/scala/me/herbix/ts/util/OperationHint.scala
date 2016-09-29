package me.herbix.ts.util

import me.herbix.ts.logic.Action.Action
import me.herbix.ts.logic.Faction.Faction
import me.herbix.ts.logic._
import me.herbix.ts.logic.card.{Card, Cards}

/**
  * Created by Chaofan on 2016/7/20.
  */
object OperationHint {

  val NOP = new OperationHint(null)

  def apply(operationType: Class[OperationModifyInfluence],
            point: Int,
            isAdd: Boolean,
            targetFaction: Faction,
            valid: (Game, Map[Country, Int]) => Boolean,
            ignoreControl: Boolean = true,
            mustAllPoints: Boolean = false,
            modifyOp: Boolean = false
           ) = {
    def realValid(game: Game, detail: Map[Country, Int]): Boolean = {
      game.calculateInfluenceCost(detail, game.playerFaction, ignoreControl) <= getPoint(game, detail) &&
        valid(game, detail) && (isAdd || detail.forall(e => game.influence(e._1, targetFaction) >= e._2)) &&
        game.excludeChina(detail) && detail.forall(e => e._1 != WorldMap.countryUS && e._1 != WorldMap.countryUSSR)
    }

    def getPoint(game: Game, pendingInfluenceChange: Map[Country, Int]): Int =
      if (modifyOp) game.modifyOp(game.playerFaction, point, pendingInfluenceChange.keys) else point

    new OperationModifyInfluenceHint(point, isAdd, targetFaction, realValid, ignoreControl, mustAllPoints, modifyOp)
  }

  private def realCardValid(valid: (Game, Card) => Boolean)(game: Game, card: Card): Boolean = {
    val data = game.currentCardData
    val cardSet = if (data != null && data.isInstanceOf[CardSet]) {
      data.asInstanceOf[CardSet]
    } else {
      game.hand(game.playerFaction)
    }
    cardSet.has(card) && valid(game, card)
  }

  def apply(operationType: Class[OperationSelectCard],
            canNull: Boolean,
            valid: (Game, Card) => Boolean
           ) =
    new OperationSelectCardHint(canNull, realCardValid(valid))

  def apply(operationType: Class[OperationSelectCards],
            valid: (Game, Card) => Boolean
           ) =
    new OperationSelectCardsHint(realCardValid(valid))

  def apply(operationType: Class[OperationSelectCardAndAction],
            presetCard: Card,
            canPlay: (Game, Card) => Boolean,
            canSpace: (Game, Card) => Boolean,
            canEvent: (Game, Card) => Boolean,
            canOperation: (Game, Card) => Boolean
           ) =
    new OperationSelectCardAndActionHint(presetCard, realCardValid(canPlay), canSpace, canEvent, canOperation)

  def apply(operationType: Class[OperationSelectOperation],
            canInfluence: (Game) => Boolean,
            canRealignment: (Game) => Boolean,
            canCoup: (Game) => Boolean
           ) =
    new OperationSelectOperationHint(canInfluence, canRealignment, canCoup)

  def apply(operationType: Class[OperationSelectCountry],
            count: Int,
            countSecondary: Int,
            valid: (Game, Set[Country]) => Boolean,
            mustAllPoints: Boolean
           ) = {

    def realValid(game: Game, detail: Set[Country]): Boolean = {
      count - detail.size >= 0 && valid(game, detail) && game.excludeChina(detail) &&
        detail.forall(c => c != WorldMap.countryUS && c != WorldMap.countryUSSR)
    }

    new OperationSelectCountryHint(count, countSecondary, realValid, mustAllPoints)
  }

  def apply(operationType: Class[OperationYesNo], isConfirm: Boolean) =
    new OperationYesNoHint(isConfirm)

  def apply(operationType: Class[OperationIntValue], min: Int, max: Int) =
    new OperationIntValueHint(min, max)

  def apply(operationType: Class[_ <: Operation]) =
    if (operationType == classOf[OperationChooseFaction]) {
      new OperationChooseFactionHint()
    } else if (operationType == classOf[OperationSelectRegion]) {
      new OperationSelectRegionHint()
    } else {
      new OperationHint(operationType)
    }

}

class OperationHint(val operationType: Class[_ <: Operation])

class OperationModifyInfluenceHint(val point: Int,
                                   val isAdd: Boolean,
                                   val targetFaction: Faction,
                                   val valid: (Game, Map[Country, Int]) => Boolean,
                                   val ignoreControl: Boolean,
                                   val mustAllPoints: Boolean,
                                   val modifyOp: Boolean
                                  ) extends OperationHint(classOf[OperationModifyInfluence]) {
  def validCountries(game: Game, detail: Map[Country, Int]): Set[Country] = {
    WorldMap.countries.values.filter(c => {
      val input = if (detail.contains(c)) {
        detail + (c -> (detail(c) + 1))
      } else {
        detail + (c -> 1)
      }
      valid(game, input) && c != WorldMap.countryUS && c != WorldMap.countryUSSR
    }).toSet
  }
}

class OperationSelectCardHint(val canNull: Boolean,
                              val valid: (Game, Card) => Boolean
                             ) extends OperationHint(classOf[OperationSelectCard]) {
  def validCards(game: Game): Set[Card] = {
    Cards.allCards.filter(valid(game, _)).toSet
  }
}

class OperationSelectCardsHint(val valid: (Game, Card) => Boolean
                              ) extends OperationHint(classOf[OperationSelectCards]) {
  def validCards(game: Game): Set[Card] = {
    Cards.allCards.filter(valid(game, _)).toSet
  }
}

class OperationSelectCardAndActionHint(val presetCard: Card,
                                       val canPlay: (Game, Card) => Boolean,
                                       val canSpace: (Game, Card) => Boolean,
                                       val canEvent: (Game, Card) => Boolean,
                                       val canOperation: (Game, Card) => Boolean
                                      ) extends OperationHint(classOf[OperationSelectCardAndAction]) {
  def validCards(game: Game): Set[Card] = {
    Cards.allCards.filter(canPlay(game, _)).toSet
  }
  def validCardActions(game: Game, card: Card): Set[Action] = {
    var result = Set.empty[Action]
    if (canSpace(game, card)) result += Action.Space
    if (canEvent(game, card)) result += Action.Event
    if (canOperation(game, card)) result += Action.Operation
    result
  }
}

class OperationSelectOperationHint(val canInfluence: (Game) => Boolean,
                                   val canRealignment: (Game) => Boolean,
                                   val canCoup: (Game) => Boolean
                                  ) extends OperationHint(classOf[OperationSelectOperation]) {
  def validOperations(game: Game): Set[Action] = {
    var result = Set.empty[Action]
    if (canInfluence(game)) result += Action.Influence
    if (canRealignment(game)) result += Action.Realignment
    if (canCoup(game)) result += Action.Coup
    result
  }
}

class OperationSelectCountryHint(val count: Int,
                                 val countSecondary: Int,
                                 val valid: (Game, Set[Country]) => Boolean,
                                 val mustAllPoints: Boolean
                                ) extends OperationHint(classOf[OperationSelectCountry]) {
  def validCountries(game: Game, detail: Set[Country]): Set[Country] = {
    WorldMap.countries.values.filter(c => {
      !detail.contains(c) && valid(game, detail + c)
    }).toSet
  }
}

class OperationYesNoHint(val isConfirm: Boolean) extends OperationHint(classOf[OperationYesNo])

class OperationIntValueHint(val min: Int, val max: Int) extends OperationHint(classOf[OperationIntValue])

class OperationChooseFactionHint extends OperationHint(classOf[OperationChooseFaction])

class OperationSelectRegionHint extends OperationHint(classOf[OperationSelectRegion])
