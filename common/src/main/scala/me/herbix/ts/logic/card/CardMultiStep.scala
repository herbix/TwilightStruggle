package me.herbix.ts.logic.card

import me.herbix.ts.logic.Faction.Faction
import me.herbix.ts.logic.State._
import me.herbix.ts.logic._
import me.herbix.ts.util.{CardCondition, CountryCondition, InfluenceCondition}

import scala.collection.mutable
import scala.reflect.ClassTag

/**
  * Created by Chaofan on 2016/7/23.
  */
abstract class CardMultiStep(id: Int, op: Int, faction: Faction, isRemovedAfterEvent: Boolean)
  extends Card(id, op, faction, isRemovedAfterEvent) {

  val stepInfo = mutable.Map.empty[Int, (State, Seq[Any], (Game, Faction, Operation) => Int)]
  var maxStep = 0

  stepInfo(0) = (null, null, (_, _, _) => 1)

  def getStep(game: Game): Int = cardEventStep.unapply(game.stateStack.elems(1)).get

  def getMetaItem[T](id: Int)(implicit game: Game): T = {
    val cardEventStep(step) = game.stateStack.elems(1)
    val meta = stepInfo(step)._2
    meta(id) match {
      case e: T => e
      case e: (Game => T) => e(game)
      case _ => null.asInstanceOf[T]
    }
  }

  def getMetaItemAsInt(id: Int)(implicit game: Game): Int = {
    val cardEventStep(step) = game.stateStack.elems(1)
    val meta = stepInfo(step)._2
    meta(id) match {
      case e: Int => e
      case e: (Game => Int) => e(game)
      case _ => 0
    }
  }


  def nextState(game: Game, faction: Faction, input: Operation): Unit = {
    val cardEventStep(step) = game.stateStack.top

    game.stateStack.pop()
    val nextStep = eventStepDone(step, game, faction, input)

    if (nextStep > maxStep) {
      game.stateStack.push(cardEventEnd)
    } else if (nextStep != -1) {
      val state = stepInfo(nextStep)._1
      game.stateStack.push(cardEventStep(nextStep))
      game.stateStack.push(state)
      state match {
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
        val validCheck: (Game, Map[Country, Int]) => Boolean = getMetaItem[Any](4) match {
          case f: (Map[Country, Int] => Boolean) => (game, detail) => f(detail)
          case f: ((Game, Map[Country, Int]) => Boolean) => f
          case f: InfluenceCondition => f.build
        }
        OperationHint(classOf[OperationModifyInfluence], getMetaItemAsInt(0), getMetaItem(1), getMetaItem(3),
          validCheck, true, getMetaItem(2))
      case State.cardEventSelectCard =>
        val stepMeta = getMetaItem[Any](0) match {
          case f: ((Game, Card) => Boolean) => f
          case f: CardCondition => f.build
        }
        OperationHint(classOf[OperationSelectCard], false, stepMeta)
      case State.cardEventSelectCardOrCancel =>
        val stepMeta = getMetaItem[Any](0) match {
          case f: ((Game, Card) => Boolean) => f
          case f: CardCondition => f.build
        }
        OperationHint(classOf[OperationSelectCard], true, stepMeta)
      case State.cardEventSelectCountry =>
        val rest = if (game.currentCardData == null) 0 else game.currentCardData.asInstanceOf[Int]
        val validCheck: (Game, Set[Country]) => Boolean = getMetaItem[Any](2) match {
          case f: (Set[Country] => Boolean) => (game, detail) => f(detail)
          case f: ((Game, Set[Country]) => Boolean) => f
          case f: CountryCondition => f.build
        }
        OperationHint(classOf[OperationSelectCountry], getMetaItemAsInt(0), rest, validCheck, getMetaItem(1))
      case State.cardEventSelectMultipleCards =>
        val stepMeta = getMetaItem[Any](0) match {
          case f: ((Game, Card) => Boolean) => f
          case f: CardCondition => f.build
        }
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

  def eventStepDone(step: Int, game: Game, faction: Faction, input: Operation): Int = {
    stepInfo(step)._3(game, faction, input)
  }

  def addStepInfo(step: Int, method: (Game, Faction, Operation) => Int, state: State, meta: Any*): Unit = {
    stepInfo(step) = (state, meta, method)
    if (step > maxStep) {
      maxStep = step
    }
  }

  def addStepInfo(step: Int, method: (Game, Faction, Operation) => Int): Unit = addStepInfo(step, method, null)
}
