package me.herbix.ts.logic.card

import me.herbix.ts.logic.Faction.Faction
import me.herbix.ts.logic.State._
import me.herbix.ts.logic._

/**
  * Created by Chaofan on 2016/7/23.
  */
abstract class CardNeedsSelection(id: Int, op: Int, faction: Faction, isRemovedAfterEvent: Boolean, steps: State*)
  extends Card(id, op, faction, isRemovedAfterEvent) {

  val stepMeta = new Array[Any](steps.length)

  def getStep(game: Game): Int = cardEventStep.unapply(game.stateStack.elems(1)).get
  def getStepMeta(game: Game): Any = {
    val cardEventStep(step) = game.stateStack.elems(1)
    stepMeta(step - 1)
  }

  def nextState(game: Game, faction: Faction, input: Operation): Unit = {
    val cardEventStep(step) = game.stateStack.top
    game.stateStack.pop()
    val nextStep = eventStepDone(step, game, faction, input)
    if (nextStep > steps.length) {
      game.stateStack.push(cardEventEnd)
    } else if (nextStep != -1) {
      game.stateStack.push(cardEventStep(nextStep))
      game.stateStack.push(steps(nextStep-1))
      steps(nextStep-1) match {
        case State.cardEventOperation =>
          game.stateStack.push(cardOperationSelect)
        case State.cardEventAnotherCard =>
          game.stateStack.push(selectAction)
        case _ =>
      }
    }
  }

  override def getOperationHint(implicit game: Game): OperationHint = {
    game.stateStack.top match {
      case State.cardEventConfirm =>
        OperationHint(classOf[OperationYesNo], true)
      case State.cardEventInfluence =>
        val stepMeta = getStepMeta(game).asInstanceOf[(Int, Boolean, Boolean, Faction, Any)]
        val validCheck: (Game, Map[Country, Int]) => Boolean = stepMeta._5 match {
          case f: (Map[Country, Int] => Boolean) => (game, detail) => f(detail)
          case f: ((Game, Map[Country, Int]) => Boolean) => f
        }
        OperationHint(classOf[OperationModifyInfluence], stepMeta._1, stepMeta._2, stepMeta._4,
          validCheck, true, stepMeta._3)
      case State.cardEventSelectCard =>
        val stepMeta = getStepMeta(game).asInstanceOf[(Game, Card) => Boolean]
        OperationHint(classOf[OperationSelectCard], false, stepMeta)
      case State.cardEventSelectCardOrCancel =>
        val stepMeta = getStepMeta(game).asInstanceOf[(Game, Card) => Boolean]
        OperationHint(classOf[OperationSelectCard], true, stepMeta)
      case State.cardEventSelectCountry =>
        val rest = if (game.currentCardData == null) 0 else game.currentCardData.asInstanceOf[Int]
        val stepMeta = getStepMeta(game).asInstanceOf[(Int, Boolean, Any)]
        val validCheck: (Game, Set[Country]) => Boolean = stepMeta._3 match {
          case f: (Set[Country] => Boolean) => (game, detail) => f(detail)
          case f: ((Game, Set[Country]) => Boolean) => f
        }
        OperationHint(classOf[OperationSelectCountry], stepMeta._1, rest, validCheck, stepMeta._2)
      case State.cardEventSelectMultipleCards =>
        val stepMeta = getStepMeta(game).asInstanceOf[(Game, Card) => Boolean]
        OperationHint(classOf[OperationSelectCards], stepMeta)
      case State.cardEventSpecial =>
        getSpecialOperationHint(game)
      case State.cardEventYesNo =>
        OperationHint(classOf[OperationYesNo], false)
      case _ =>
        OperationHint.NOP
    }
  }

  def getSpecialOperationHint(game: Game): OperationHint = OperationHint.NOP

  /**
    * do something after each step
    *
    * @param step step number, 0 if start
    * @param game game instance
    * @param faction US or USSR
    * @param input operation
    * @return next step
    *         -1 if handle state stack by card
    */
  def eventStepDone(step: Int, game: Game, faction: Faction, input: Operation): Int

}
