package me.herbix.ts.logic.card

import me.herbix.ts.logic.Faction.{Faction, _}
import me.herbix.ts.logic.Region._
import me.herbix.ts.logic.State._
import me.herbix.ts.logic._
import me.herbix.ts.logic.chinesecivilwar.CCWFlags
import me.herbix.ts.util.ConditionBuilder._
import me.herbix.ts.util.{HistoryCardAction, OperationHint}

/**
  * Created by Chaofan on 2017/4/22.
  */
object CardP01NonAlignMovement extends CardMultiStep(1 + PromoCards.Offset, 1, Neutral, false) {
  @step1(cardEventSelectCountry, 1, false,
    selectCountry.withName("India").or.inRegion(Africa, SouthEastAsia, MidEast, SouthAmerica)
      .influenceMoreOrEqual(US, 2).influenceMoreOrEqual(USSR, 2))
  def removeInfluence(game: Game, input: Operation): Unit = {
    val countries = input.asInstanceOf[OperationSelectCountry].detail
    game.modifyInfluence(US, false, countries.map(c => (c, game.influence(c, US))).toMap)
    game.modifyInfluence(USSR, false, countries.map(c => (c, game.influence(c, USSR))).toMap)
    for (i <- 1 to 4) {
      game.discardCard(game.pickCardFromDeck(), game.operatingPlayer, true, true)
    }
  }
}

object CardP02Mobutu extends CardInstant(2 + PromoCards.Offset, 2, US, true) {
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    game.theWorldMap.replaceWithHighStabilityZaire()
    game.addFlag(Neutral, Flags.Mobutu)
    game.modifyInfluence(US, true, Map(game.theWorldMap.countries("Zaire") -> 2))
    true
  }
}

object CardP03BerlinWall extends CardInstant(3 + PromoCards.Offset, 2, USSR, true) {
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    game.modifyInfluence(USSR, true, Map(game.theWorldMap.countries("E.Germany") -> 2))
    if (game.space(USSR).level <= game.space(US).level) {
      game.increaseSpace(USSR, 1)
    }
    true
  }
}

object CardP04Stanislav extends CardMultiStep(4 + PromoCards.Offset, 3, Neutral, true) {
  override def canEvent(game: Game, faction: Faction) = game.defcon == 2

  @step1(cardEventSpecial)
  def setDefconAndMilitary(game: Game, input: Operation): Unit = {
    game.setDefcon(input.asInstanceOf[OperationIntValue].value)
  }

  override def getSpecialOperationHint(game: Game): OperationHint = OperationHint(classOf[OperationIntValue], 4, 5)
}

object CardP05KremlinFlu extends CardMultiStep(5 + PromoCards.Offset, 2, US, false) {
  @prepare
  def addFlag(game: Game): Unit = {
    game.addFlag(USSR, Flags.KremlinFlu)
  }

  @step1(cardEventOperation)
  def operationDone(game: Game): Unit = {
    /* do nothing */
  }
}

object CardP06FirstLightning extends CardMultiStep(6 + PromoCards.Offset, 2, USSR, true) {
  class CardP06FirstLightningDummy(tmpop: Int) extends Card(6 + PromoCards.Offset, op, Neutral, false) {
    override val op = tmpop
    override def modifyOp(game: Game, faction: Faction, originalOp: Int): Int = op
    override def nextState(game: Game, faction: Faction, input: Operation): Unit =
      CardP06FirstLightning.nextState(game, faction, input)
  }
  object CardP06FirstLightningDummy {
    val map = (0 to 4).map(i => i -> new CardP06FirstLightningDummy(i)).toMap
    def apply(op: Int) = map(op)
  }
  override def canEvent(game: Game, faction: Faction) = game.hand(USSR).exists(_.faction == US)

  @step1(cardEventSelectCard, selectCard.isOppositeCard)
  def selectOpponentCard(game: Game, faction: Faction, input: Operation): Int = {
    if (input.asInstanceOf[OperationSelectCard].card.isEmpty) {
      3
    } else {
      val card = input.asInstanceOf[OperationSelectCard].card.get
      val op = game.modifyOp(faction, card.op)
      game.handRemove(faction, card)
      game.recordHistory(new HistoryCardAction(faction, card, Action.Operation, false))
      game.discardCard(card, faction, true)
      game.currentCardChange(CardP06FirstLightningDummy(op))
      2
    }
  }

  @step2(cardEventOperation)
  def operationDone(game: Game, input: Operation): Unit = {
    game.currentCard.afterPlay(game, faction)
    game.currentCardRollBack()
  }

  override def afterPlay(game: Game, faction: Faction): Unit = {
    game.setDefcon(game.defcon - 1)
  }
}

object CardP07WhoLostChina extends CardInstant(7 + PromoCards.Offset, 1, USSR, false) {
  override def canEvent(game: Game, faction: Faction) = !game.flags.hasFlag(CCWFlags.ChineseCivilWar)
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    if (game.hand(USSR).has(game.theCards.chinaCard)) {
      game.setMilitary(US, 0)
    }
    true
  }
}

object CardP08DoNotWaitForTheTranslation extends CardInstant(8 + PromoCards.Offset, 3, US, true) {
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    if (game.military(US) < game.military(USSR)) {
      game.addVp(US, 2)
    }
    true
  }
}

object PromoCards {
  val Offset = 150
}
