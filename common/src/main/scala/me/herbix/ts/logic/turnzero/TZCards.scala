package me.herbix.ts.logic.turnzero

import me.herbix.ts.logic.Faction.Faction
import me.herbix.ts.logic._
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

  replaceCard(Card007SocialistGovernments, Card007SocialistGovernmentsTZ)
  replaceCard(Card010Blockade, Card010BlockadeTZ)
  replaceCard(Card028SuezCrisis, Card028SuezCrisisTZ)

  var earlyWarSetValue = super.earlyWarSet.toSet
  var midWarSetValue = super.midWarSet.toSet

  def reset(): Unit = {
    earlyWarSetValue = super.earlyWarSet.toSet
    midWarSetValue = super.midWarSet.toSet
  }

  override def earlyWarSet = earlyWarSetValue
  override def midWarSet = midWarSetValue

  override def isEarlyWarCard(card: Card): Boolean = {
    val i = card.id
    super.isEarlyWarCard(card) || (i >= IdInc + 1 && i <= IdInc + 2)
  }

  def usStateCraft = cardMap.filter(e => e._1 <= IdInc + 12 && e._1 >= IdInc + 8).values
  def ussrStateCraft = cardMap.filter(e => e._1 <= IdInc + 7 && e._1 >= IdInc + 3).values
}

class CardStateCraft(id: Int, faction: Faction, val modifier: Int, val cancelEffect: Boolean = false)
  extends CardInstant(id + TZCards.IdInc + 2, 0, faction, false) {
  override def instantEvent(game: Game, faction: Faction): Boolean = true /* Do nothing */
  def isDummy = modifier == 0 && !cancelEffect
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

object Card010BlockadeTZ extends CardMultiStep(10, 1, USSR, true) {
  override def canEvent(game: Game, faction: Faction) = !game.flags.hasFlag(TZFlags.blockadeNegated)

  @prepare
  def changeOperatingPlayer(game: Game): Unit = {
    game.operatingPlayerChange(US)
  }

  @step1(cardEventSelectCardOrCancel, selectCard.minModifiedOp(US, 3).canDiscardBy(US))
  def discardOrRemoveUSInfluence(game: Game, input: Operation): Unit = {
    val op = input.asInstanceOf[OperationSelectCard]
    if (op.card.isDefined) {
      val card = op.card.get
      game.handRemove(US, card)
      game.discardCard(card, US, true, true)
    } else {
      val wGermany = game.theWorldMap.countries("W.Germany")
      game.modifyInfluence(US, false, Map(wGermany -> game.influence(wGermany, US)))
    }
    game.operatingPlayerRollBack()
  }

  Card028SuezCrisis
}

object Card007SocialistGovernmentsTZ extends CardMultiStep(7, 3, USSR, false) {
  override def canEvent(game: Game, faction: Faction) = !game.flags.hasFlag(Flags.IronLady) &&
    !game.flags.hasFlag(TZFlags.socialistGovernmentsNegated)

  @step1(cardEventInfluence, 3, false, false, US, influence.inRegion(Region.WestEurope).max(2))
  def removeUSInfluence(game: Game, input: Operation): Unit = {
    game.modifyInfluence(US, false, input.asInstanceOf[OperationModifyInfluence].detail)
  }
}

object Card028SuezCrisisTZ extends CardMultiStep(28, 3, USSR, true) {
  override def canEvent(game: Game, faction: Faction) = !game.flags.hasFlag(TZFlags.suezCrisisNegated)

  @step1(cardEventInfluence, 4, false, false, US, influence.inCountry("UK", "France", "Israel").max(2))
  def removeUSInfluence(game: Game, input: Operation): Unit = {
    game.modifyInfluence(US, false, input.asInstanceOf[OperationModifyInfluence].detail)
  }
}
