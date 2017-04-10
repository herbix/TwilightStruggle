package me.herbix.ts.logic.turnzero

import me.herbix.ts.logic.{OperationModifyInfluence, Operation, Game, Region}
import me.herbix.ts.logic.State._
import me.herbix.ts.logic.card._
import me.herbix.ts.logic.Faction._
import me.herbix.ts.util.ConditionBuilder._

/**
  * Created by Chaofan on 2017/4/4.
  */
object TZCards extends CardsTrait {
  final val IdInc = 200

  addCard(CardTZ01EuropeScoring)
  addCard(CardTZ02NationalistChina)
  addCard(CardTZ03Andropov)
  addCard(CardTZ04Khrushchev)
  addCard(CardTZ05Molotov)
  addCard(CardTZ06Beria)
  addCard(CardTZ07KGB)
  addCard(CardTZ08Kennan)
  addCard(CardTZ09Acheson)
  addCard(CardTZ10Marshall)
  addCard(CardTZ11Dulles)
  addCard(CardTZ12OSS)

  var earlyWarSetValue = super.earlyWarSet
  var midWarSetValue = super.midWarSet

  def reset(): Unit = {
    earlyWarSetValue = super.earlyWarSet
    midWarSetValue = super.midWarSet
  }

  override def earlyWarSet = earlyWarSetValue
  override def midWarSet = midWarSetValue

  override def allCards = cardMap.filter(e => e._1 < IdInc + 3).values

  override def isEarlyWarCard(card: Card): Boolean = {
    val i = card.id
    i <= 35 || (i >= 103 && i <= 106) || (i >= IdInc + 1 && i <= IdInc + 2)
  }
}

class CardStateCraft(id: Int, faction: Faction, val modifier: Int, val cancelEffect: Boolean = false)
  extends CardInstant(id + TZCards.IdInc + 2, 0, faction, false) {
  override def instantEvent(game: Game, faction: Faction): Boolean = true /* Do nothing */
}

object CardTZ01EuropeScoring extends CardScoring(TZCards.IdInc + 1, Region.Europe)

object CardTZ02NationalistChina extends CardMultiStep(TZCards.IdInc + 2, 2, US, false) {
  @step1(cardEventInfluence, 3, true, true, US, influence.inRegion(Region.Asia).max(1))
  def addUSInfluence(game: Game, input: Operation): Unit = {
    game.modifyInfluence(US, true, input.asInstanceOf[OperationModifyInfluence].detail)
  }
}

object CardTZ03Andropov extends CardStateCraft(1, USSR, -1)
object CardTZ04Khrushchev extends CardStateCraft(2, USSR, -1)
object CardTZ05Molotov extends CardStateCraft(3, USSR, -2)
object CardTZ06Beria extends CardStateCraft(4, USSR, 0, true)
object CardTZ07KGB extends CardStateCraft(5, USSR, 0)

object CardTZ08Kennan extends CardStateCraft(6, US, 1)
object CardTZ09Acheson extends CardStateCraft(7, US, 1)
object CardTZ10Marshall extends CardStateCraft(8, US, 2)
object CardTZ11Dulles extends CardStateCraft(9, US, 0, true)
object CardTZ12OSS extends CardStateCraft(10, US, 0)
