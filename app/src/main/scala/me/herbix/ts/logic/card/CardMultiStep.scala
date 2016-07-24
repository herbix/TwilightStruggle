package me.herbix.ts.logic.card

import me.herbix.ts.logic.Faction.Faction
import me.herbix.ts.logic.State._
import me.herbix.ts.logic._

import scala.collection.mutable

/**
  * Created by Chaofan on 2016/7/23.
  */
abstract class CardMultiStep(id: Int, op: Int, faction: Faction, isRemovedAfterEvent: Boolean)
  extends Card(id, op, faction, isRemovedAfterEvent) {

  val (
    steps: Array[State],
    stepMeta: Array[Seq[Any]],
    stepMethods: Array[(Game, Faction, Operation) => Int]
    ) = initSteps()

  def getMetaItem[T](id: Int)(implicit game: Game): T = {
    val cardEventStep(step) = game.stateStack.elems(1)
    val meta = stepMeta(step)
    meta(id) match {
      case e: T => e
      case e: (Game => T) => e(game)
      case _ => null.asInstanceOf[T]
    }
  }

  def nextState(game: Game, faction: Faction, input: Operation): Unit = {
    val cardEventStep(step) = game.stateStack.top

    game.stateStack.pop()
    val nextStep = eventStepDone(step, game, faction, input)

    if (nextStep > steps.length - 1) {
      game.stateStack.push(cardEventEnd)
    } else if (nextStep != -1) {
      game.stateStack.push(cardEventStep(nextStep))
      game.stateStack.push(steps(nextStep))
      steps(nextStep) match {
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
        }
        OperationHint(classOf[OperationModifyInfluence], getMetaItem(0), getMetaItem(1), getMetaItem(3),
          validCheck, true, getMetaItem(2))
      case State.cardEventSelectCard =>
        val stepMeta = getMetaItem[(Game, Card) => Boolean](0)
        OperationHint(classOf[OperationSelectCard], false, stepMeta)
      case State.cardEventSelectCardOrCancel =>
        val stepMeta = getMetaItem[(Game, Card) => Boolean](0)
        OperationHint(classOf[OperationSelectCard], true, stepMeta)
      case State.cardEventSelectCountry =>
        val rest = if (game.currentCardData == null) 0 else game.currentCardData.asInstanceOf[Int]
        val validCheck: (Game, Set[Country]) => Boolean = getMetaItem[Any](2) match {
          case f: (Set[Country] => Boolean) => (game, detail) => f(detail)
          case f: ((Game, Set[Country]) => Boolean) => f
        }
        OperationHint(classOf[OperationSelectCountry], getMetaItem(0), rest, validCheck, getMetaItem(1))
      case State.cardEventSelectMultipleCards =>
        val stepMeta = getMetaItem[(Game, Card) => Boolean](0)
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
    stepMethods(step)(game, faction, input)
  }

  private def initSteps(): (Array[State], Array[Seq[Any]], Array[(Game, Faction, Operation) => Int]) = {/*
    val targets =
      getClass.getMethods
          .toStream
          .filter(m => m.getDeclaredAnnotations.exists(_.isInstanceOf[StepAnnotation]))
          .filter(m => m.getParameterTypes.equals(Array[Class[_]](classOf[Game], classOf[Faction], classOf[Operation])))
          .map(m => (m, m.getDeclaredAnnotations.find(_.isInstanceOf[StepAnnotation]).get.asInstanceOf[StepAnnotation]))
          .toList

    println(getClass.getMethods.find(_.getName == "step1").get.getAnnotations.toList)
    println(targets)

    val methodMap = mutable.Map.empty[Int, (State, Seq[Any], (Game, Faction, Operation) => Int)]

    for ((method, annotation) <- targets) {
      val step = annotation.step
      val state = annotation.state
      val meta = annotation.meta
      def callbackInt(game: Game, faction: Faction, operation: Operation): Int = {
        method.invoke(this, game, faction, operation).asInstanceOf[Int]
      }
      def callbackUnit(game: Game, faction: Faction, operation: Operation): Int = {
        method.invoke(this, game, faction, operation)
        step + 1
      }
      if (method.getReturnType == classOf[Int]) {
        methodMap(step) = (state, meta, callbackInt)
      } else {
        methodMap(step) = (state, meta, callbackUnit)
      }
    }

    def callbackPrepare(game: Game, faction: Faction, operation: Operation): Int = 1

    if (!methodMap.contains(0)) {
      methodMap(0) = (null, Seq.empty[Any], callbackPrepare)
    }

    assert(0 until methodMap.size forall methodMap.contains)
*/
  val methodMap = mutable.Map.empty[Int, (State, Seq[Any], (Game, Faction, Operation) => Int)]
    val methodList = methodMap.toList.sortBy(_._1)

    (methodList.map(_._2._1).toArray, methodList.map(_._2._2).toArray, methodList.map(_._2._3).toArray)
  }
}
