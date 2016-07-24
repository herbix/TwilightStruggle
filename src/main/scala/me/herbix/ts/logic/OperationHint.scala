package me.herbix.ts.logic


import me.herbix.ts.logic.Faction._
import me.herbix.ts.logic.card.Card

/**
  * Created by Chaofan on 2016/7/20.
  */
object OperationHint {

  val NOP = new OperationHint(null)
  val CHOOSE_FACTION = new OperationHint(classOf[OperationChooseFaction])
  val SELECT_REGION = new OperationHint(classOf[OperationSelectRegion])

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
        valid(game, detail) && (isAdd || detail.forall(e => game.influence(e._1, targetFaction) >= e._2))
    }

    def getPoint(game: Game, pendingInfluenceChange: Map[Country, Int]): Int =
      if (modifyOp) game.modifyOp(game.playerFaction, point, pendingInfluenceChange.keys) else point

    new OperationModifyInfluenceHint(point, isAdd, targetFaction, realValid, ignoreControl, mustAllPoints, modifyOp)
  }

  def apply(operationType: Class[OperationSelectCard],
            canNull: Boolean,
            valid: (Game, Card) => Boolean
           ) =
    new OperationSelectCardHint(canNull, valid)

  def apply(operationType: Class[OperationSelectCards],
            valid: (Game, Card) => Boolean
           ) =
    new OperationSelectCardsHint(valid)

  def apply(operationType: Class[OperationSelectCardAndAction],
            presetCard: Card,
            canPlay: (Game, Card) => Boolean,
            canSpace: (Game, Card) => Boolean,
            canEvent: (Game, Card) => Boolean,
            canOperation: (Game, Card) => Boolean
           ) =
    new OperationSelectCardAndActionHint(presetCard, canPlay, canSpace, canEvent, canOperation)

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
      count - detail.size >= 0 && valid(game, detail)
    }

    new OperationSelectCountryHint(count, countSecondary, realValid, mustAllPoints)
  }

  def apply(operationType: Class[OperationYesNo], isConfirm: Boolean) =
    new OperationYesNoHint(isConfirm)

  def apply(operationType: Class[OperationIntValue], min: Int, max: Int) =
    new OperationIntValueHint(min, max)

}

class OperationHint(val operationType: Class[_ <: Operation])

class OperationModifyInfluenceHint(val point: Int,
                                   val isAdd: Boolean,
                                   val targetFaction: Faction,
                                   val valid: (Game, Map[Country, Int]) => Boolean,
                                   val ignoreControl: Boolean,
                                   val mustAllPoints: Boolean,
                                   val modifyOp: Boolean
                                  ) extends OperationHint(classOf[OperationModifyInfluence])

class OperationSelectCardHint(val canNull: Boolean,
                              val valid: (Game, Card) => Boolean
                             ) extends OperationHint(classOf[OperationSelectCard])

class OperationSelectCardsHint(val valid: (Game, Card) => Boolean
                              ) extends OperationHint(classOf[OperationSelectCards])

class OperationSelectCardAndActionHint(val presetCard: Card,
                                       val canPlay: (Game, Card) => Boolean,
                                       val canSpace: (Game, Card) => Boolean,
                                       val canEvent: (Game, Card) => Boolean,
                                       val canOperation: (Game, Card) => Boolean
                                      ) extends OperationHint(classOf[OperationSelectCardAndAction])

class OperationSelectOperationHint(val canInfluence: (Game) => Boolean,
                                   val canRealignment: (Game) => Boolean,
                                   val canCoup: (Game) => Boolean
                                  ) extends OperationHint(classOf[OperationSelectOperation])

class OperationSelectCountryHint(val count: Int,
                                 val countSecondary: Int,
                                 val valid: (Game, Set[Country]) => Boolean,
                                 val mustAllPoints: Boolean
                                ) extends OperationHint(classOf[OperationSelectCountry])

class OperationYesNoHint(val isConfirm: Boolean) extends OperationHint(classOf[OperationYesNo])

class OperationIntValueHint(val min: Int, val max: Int) extends OperationHint(classOf[OperationIntValue])
