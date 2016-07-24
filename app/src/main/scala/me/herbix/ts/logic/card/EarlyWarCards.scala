package me.herbix.ts.logic.card

import me.herbix.ts.logic.Faction.{Faction, _}
import me.herbix.ts.logic.Region.{Region, RegionState}
import me.herbix.ts.logic.State._
import me.herbix.ts.logic._

import me.herbix.ts.util.ConditionBuilder._

import scala.collection.mutable

/**
  * Created by Chaofan on 2016/7/24.
  */
object Card001AsiaScoring extends CardScoring(1, Region.Asia)
object Card002EuropeScoring extends CardScoring(2, Region.Europe)
object Card003MidEastScoring extends CardScoring(3, Region.MidEast)

object Card004DuckNCover extends CardInstant(4, 3, US, false) {
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    game.setDefcon(game.defcon - 1)
    game.addVpAndCheck(US, 5 - game.defcon)
    true
  }
}

object Card005FiveYearPlan extends CardInstant(5, 3, US, false) {
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    val ussrHand = game.hand(USSR)
    if (ussrHand.isEmptyExcludingChinaCard) {
      return true
    }
    val card = ussrHand.pickAndRemove(game.random)
    val activateEvent = card.faction == US && card.canEvent(game, US)
    game.discardCard(card, USSR, !activateEvent, true)
    if (activateEvent) {
      game.currentCard = card
      game.operatingPlayer = US
      game.recordHistory(new HistoryEvent(US, game.currentCard))
      card.nextState(game, US, null)
      false
    } else {
      true
    }
  }
}

object Card006ChinaCard extends CardInstant(6, 4, Neutral, false) {
  override def canEvent(game: Game, faction: Faction) = false
  override def canPlay(game: Game, faction: Faction) =
    super.canPlay(game, faction) && !game.flags.hasFlag(faction, Flags.CantPlayChinaCard)
  override def canDiscard(game: Game, faction: Faction) = false
  override def modifyOp(game: Game, faction: Faction, originalOp: Int, targets: Iterable[Country]): Int =
    if (targets.forall(_.regions(Region.Asia))) {
      originalOp + 1
    } else {
      originalOp
    }
  override def afterPlay(game: Game, faction: Faction): Unit = {
    if (faction == US) {
      game.removeFlag(US, Flags.Taiwan)
    }
  }
  override def instantEvent(game: Game, faction: Faction): Boolean = throw new NotImplementedError()
}

object Card007SocialistGovernments extends CardMultiStep(7, 3, USSR, false) {
  override def canEvent(game: Game, faction: Faction) = !game.flags.hasFlag(Flags.IronLady)

  @step1(cardEventInfluence, 3, false, false, US, influence.inRegion(Region.WestEurope).max(2))
  def removeUSInfluence(game: Game, input: Operation): Unit = {
    game.modifyInfluence(US, false, input.asInstanceOf[OperationModifyInfluence].detail)
  }
}

object Card008Fidel extends CardInstant(8, 2, USSR, true) {
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    val cuba = WorldMap.countries("Cuba")
    game.modifyInfluence(US, false, Map(cuba -> game.influence(cuba, US)))
    game.modifyInfluence(USSR, true, Map(cuba -> cuba.stability))
    true
  }
}

object Card009VietnamRevolts extends CardInstant(9, 2, USSR, true) {
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    val vietnam = WorldMap.countries("Vietnam")
    game.modifyInfluence(USSR, true, Map(vietnam -> 2))
    game.addFlag(USSR, Flags.VietnamRevolts)
    true
  }
}

object Card010Blockade extends CardMultiStep(10, 1, USSR, true) {
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
      val wGermany = WorldMap.countries("W.Germany")
      game.modifyInfluence(US, false, Map(wGermany -> game.influence(wGermany, US)))
    }
    game.operatingPlayerRollBack()
  }
}

object Card011KoreanWar extends CardInstant(11, 2, USSR, true) {
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    val southKorea = WorldMap.countries("S.Korea")
    val modifier = southKorea.adjacentCountries.count(_.getController(game) == US)
    game.war(USSR, southKorea, modifier, 4, 2, 2)
    true
  }
  override def afterPlay(game: Game, faction: Faction): Unit = {
    Card059FlowerPower.triggerFlowerPowerEffect(game, faction)
  }
}

object Card012RomanianAbdication extends CardInstant(12, 1, USSR, true) {
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    val romania = WorldMap.countries("Romania")
    game.modifyInfluence(US, false, Map(romania -> game.influence(romania, US)))
    game.modifyInfluence(USSR, true, Map(romania -> romania.stability))
    true
  }
}

object Card013ArabIsraeliWar extends CardInstant(13, 2, USSR, false) {
  override def canEvent(game: Game, faction: Faction) = !game.flags.hasFlag(Flags.CampDavid)
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    val israel = WorldMap.countries("Israel")
    val modifier = israel.adjacentCountries.count(_.getController(game) == US) +
      (if (israel.getController(game) == US) 1 else 0)
    game.war(USSR, israel, modifier, 4, 2, 2)
    true
  }
  override def afterPlay(game: Game, faction: Faction): Unit = {
    Card059FlowerPower.triggerFlowerPowerEffect(game, faction)
  }
}

object Card014COMECON extends CardMultiStep(14, 3, USSR, true) {
  @step1(cardEventInfluence, 4, true, false, USSR, influence.inRegion(Region.EastEurope).notControlledBy(US).max(1))
  def addUSSRInfluence(game: Game, input: Operation): Unit = {
    game.modifyInfluence(USSR, true, input.asInstanceOf[OperationModifyInfluence].detail)
  }
}

object Card015Nasser extends CardInstant(15, 1, USSR, true) {
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    val egypt = WorldMap.countries("Egypt")
    game.modifyInfluence(US, false, Map(egypt -> (game.influence(egypt, US) + 1) / 2))
    game.modifyInfluence(USSR, true, Map(egypt -> 2))
    true
  }
}

object Card016WarsawPact extends CardMultiStep(16, 3, USSR, true) {
  @step1(cardEventYesNo)
  def removeUSOrAddUSSR(game: Game, input: Operation): Int = {
    if (input.asInstanceOf[OperationYesNo].value) 2 else 3
  }

  @step2(cardEventSelectCountry, 4, false, selectCountry.inRegion(Region.EastEurope))
  def removeUSInfluence(game: Game, input: Operation): Int = {
    game.modifyInfluence(US, false, input.asInstanceOf[OperationSelectCountry].detail.map(country =>
      country -> game.influence(country, US)
    ).toMap)
    game.addFlag(US, Flags.WarsawPact)
    4
  }

  @step3(cardEventInfluence, 5, true, true, USSR, influence.inRegion(Region.EastEurope).max(2))
  def addUSSRInfluence(game: Game, input: Operation): Int = {
    game.modifyInfluence(USSR, true, input.asInstanceOf[OperationModifyInfluence].detail)
    game.addFlag(US, Flags.WarsawPact)
    4
  }
}

object Card017DeGaulle extends CardInstant(17, 3, USSR, true) {
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    val france = WorldMap.countries("France")
    game.modifyInfluence(US, false, Map(france -> 2))
    game.modifyInfluence(USSR, true, Map(france -> 1))
    game.addFlag(USSR, Flags.DeGaulle)
    true
  }
}

object Card018CaptureNazi extends CardInstant(18, 1, Neutral, true) {
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    game.increaseSpace(faction, 1)
    true
  }
}

object Card019TrumanDoctrine extends CardMultiStep(19, 1, US, true) {
  @step1(cardEventSelectCountry, 1, false, selectCountry.inRegion(Region.Europe).notControlled)
  def removeUSSRInfluence(game: Game, input: Operation): Unit = {
    val country = input.asInstanceOf[OperationSelectCountry].detail.head
    game.modifyInfluence(USSR, false, Map(country -> game.influence(country, USSR)))
  }
}

object Card020OlympicGames extends CardMultiStep(20, 2, Neutral, false) {

  object Card020OlympicGamesDummy extends Card(20, 4, Neutral, false) {
    override def modifyOp(game: Game, faction: Faction, originalOp: Int): Int = op
    override def nextState(game: Game, faction: Faction, input: Operation): Unit =
      Card020OlympicGames.nextState(game, faction, input)
  }

  @prepare
  def changeOperatingPlayer(game: Game): Unit = game.operatingPlayerChange(Faction.getOpposite(faction))

  @step1(cardEventYesNo)
  def decideAttendance(game: Game, input: Operation): Int = {
    game.operatingPlayerRollBack()
    if (input.asInstanceOf[OperationYesNo].value) {
      var winner = Neutral
      do {
        val myDice = game.rollDice()
        val oppositeDice = game.rollDice()
        val myself = game.operatingPlayer
        val opponent = Faction.getOpposite(game.operatingPlayer)
        game.recordHistory(new HistoryRollDice(myself, myDice, 2))
        game.recordHistory(new HistoryRollDice(opponent, oppositeDice))
        if (myDice + 2 > oppositeDice) {
          winner = game.operatingPlayer
        } else if (myDice + 2 < oppositeDice) {
          winner = Faction.getOpposite(game.operatingPlayer)
        }
      } while (winner == Neutral)
      game.addVpAndCheck(winner, 2)
      3
    } else {
      game.setDefcon(game.defcon - 1)
      game.currentCard = Card020OlympicGamesDummy
      2
    }
  }

  @step2(cardEventOperation)
  def operationDone(): Unit = {}
}

object Card021NATO extends CardInstant(21, 4, US, true) {
  override def canEvent(game: Game, faction: Faction): Boolean =
    game.flags.hasFlag(Flags.WarsawPact) || game.flags.hasFlag(Flags.MarshallPlan)
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    game.addFlag(USSR, Flags.NATO)
    true
  }
}

object Card022IndependentReds extends CardMultiStep(22, 2, US, true) {
  val countryNames = Set("Yugoslavia", "Romania", "Bulgaria", "Hungary", "Czechoslovakia")

  @step1(cardEventSelectCountry, 1, true, selectCountry.withName(countryNames))
  def addEnoughUSInfluence(game: Game, input: Operation): Unit = {
    val country = input.asInstanceOf[OperationSelectCountry].detail.head
    val add = Math.max(0, game.influence(country, USSR) - game.influence(country, US))
    game.modifyInfluence(US, true, Map(country -> add))
  }
}

object Card023MarshallPlan extends CardMultiStep(23, 4, US, true) {
  @step1(cardEventInfluence, 7, true, false, US, influence.inRegion(Region.WestEurope).notControlledBy(USSR).max(1))
  def addUSInfluence(game: Game, input: Operation): Unit = {
    game.modifyInfluence(US, true, input.asInstanceOf[OperationModifyInfluence].detail)
    game.addFlag(US, Flags.MarshallPlan)
  }
}

object Card024IndoPakistaniWar extends CardMultiStep(24, 2, Neutral, false) {
  @step1(cardEventSelectCountry, 1, true, selectCountry.withName("India", "Pakistan"))
  def eventStepDone(game: Game, input: Operation): Unit = {
    val country = input.asInstanceOf[OperationSelectCountry].detail.head
    val opposite = Faction.getOpposite(faction)
    val modifier = country.adjacentCountries.count(_.getController(game) == opposite)
    game.war(faction, country, modifier, 4, 2, 2)
  }

  override def afterPlay(game: Game, faction: Faction): Unit = {
    Card059FlowerPower.triggerFlowerPowerEffect(game, faction)
  }
}

object Card025Containment extends CardInstant(25, 3, US, true) {
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    game.addFlag(US, Flags.Containment)
    true
  }
}

object Card026CIACreated extends CardMultiStep(26, 1, US, true) {
  override def modifyOp(game: Game, faction: Faction, originalOp: Int): Int = {
    if (game.currentCardData == true) op else originalOp
  }

  @prepare
  def prepareShowHand(game: Game): Unit = {
    game.currentCardData = game.hand(USSR)
    game.clearSnapshots()
  }

  @step1(cardEventConfirm)
  def handConfirmed(game: Game): Unit = {
    game.currentCardData = true
  }

  @step2(cardEventOperation)
  def operationDone(game: Game): Unit = {
    game.currentCardData = null
  }
}

object Card027USJapanPact extends CardInstant(27, 4, US, true) {
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    val japan = WorldMap.countries("Japan")
    val change = game.influence(japan, USSR) + japan.stability - game.influence(japan, US)
    if (change > 0) {
      game.modifyInfluence(US, true, Map(japan -> change))
    }
    game.addFlag(USSR, Flags.USJapanPact)
    true
  }
}

object Card028SuezCrisis extends CardMultiStep(28, 3, USSR, true) {
  @step1(cardEventInfluence, 4, false, false, US, influence.inCountry("UK", "France", "Israel").max(2))
  def removeUSInfluence(game: Game, input: Operation): Unit = {
    game.modifyInfluence(US, false, input.asInstanceOf[OperationModifyInfluence].detail)
  }
}

object Card029EastEuropeanUnrest extends CardMultiStep(29, 3, US, false) {
  @step1(cardEventSelectCountry, 3, true, selectCountry.inRegion(Region.EastEurope))
  def removeUSSRInfluence(game: Game, input: Operation): Unit = {
    game.modifyInfluence(USSR, false, input.asInstanceOf[OperationSelectCountry].detail.map(country =>
      country -> (if (game.turn < 8) 1 else 2)
    ).toMap)
  }
}

object Card030Decolonization extends CardMultiStep(30, 2, USSR, false) {
  @step1(cardEventInfluence, 4, true, true, USSR, influence.inRegion(Region.Africa, Region.SouthEastAsia).max(1))
  def addUSSRInfluence(game: Game, input: Operation): Unit = {
    game.modifyInfluence(USSR, true, input.asInstanceOf[OperationModifyInfluence].detail)
  }
}

object Card031RedScarePurge extends CardInstant(31, 4, Neutral, false) {
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    game.addFlag(Faction.getOpposite(faction), Flags.RedScarePurge)
    true
  }
}

object Card032UNIntervention extends CardMultiStep(32, 1, Neutral, false) {
  class Card032UNInterventionDummy(tmpop: Int) extends Card(32, op, Neutral, false) {
    override val op = tmpop
    override def modifyOp(game: Game, faction: Faction, originalOp: Int): Int = op
    override def nextState(game: Game, faction: Faction, input: Operation): Unit =
      Card032UNIntervention.nextState(game, faction, input)
  }
  object Card032UNInterventionDummy {
    val map = (0 to 4).map(i => i -> new Card032UNInterventionDummy(i)).toMap
    def apply(op: Int) = map(op)
  }
  override def canHeadline(game: Game, faction: Faction) = false
  override def canEvent(game: Game, faction: Faction) =
    game.hand(faction).exists(_.faction == Faction.getOpposite(faction))

  @step1(cardEventSelectCard, selectCard.isOppositeCard)
  def selectOpponentCard(game: Game, input: Operation): Unit = {
    val card = input.asInstanceOf[OperationSelectCard].card.get
    val op = game.modifyOp(faction, card.op)
    game.handRemove(faction, card)
    game.recordHistory(new HistoryCardAction(faction, card, Action.Operation, false))
    game.discardCard(card, faction, true)
    game.currentCardChange(Card032UNInterventionDummy(op))
  }

  @step2(cardEventOperation)
  def operationDone(game: Game, input: Operation): Unit = {
    game.currentCard.afterPlay(game, faction)
    game.currentCardRollBack()
  }

  override def afterPlay(game: Game, faction: Faction): Unit = {
    if (game.flags.hasFlag(USSR, Flags.U2Incident)) {
      game.addVpAndCheck(USSR, 1)
      game.removeFlag(USSR, Flags.U2Incident)
    }
  }
}

object Card033DeStalinization extends CardMultiStep(33, 3, USSR, true) {
  @step1(cardEventInfluence, 4, false, false, USSR, influence.all)
  def removeUSSRInfluence(game: Game, input: Operation): Unit = {
    val detail = input.asInstanceOf[OperationModifyInfluence].detail
    game.modifyInfluence(USSR, false, detail)
    game.currentCardData = detail.toStream.map(_._2).sum
  }

  @step2(cardEventInfluence, (g: Game) => g.currentCardData.asInstanceOf[Int], true, false, USSR, influence.notControlledBy(US).max(2))
  def addUSSRInfluence(game: Game, input: Operation): Unit = {
    game.modifyInfluence(USSR, true, input.asInstanceOf[OperationModifyInfluence].detail)
    game.currentCardData = null
  }
}

object Card034NuclearTestBan extends CardInstant(34, 4, Neutral, false) {
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    game.addVpAndCheck(faction, game.defcon - 2)
    game.setDefcon(game.defcon + 2)
    true
  }
}

object Card035Taiwan extends CardInstant(35, 2, US, true) {
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    game.addFlag(Faction.US, Flags.Taiwan)
    true
  }
}

object Card103Defectors extends CardInstant(103, 2, US, false) {
  override def canHeadline(game: Game, faction: Faction) = true
  override def canEvent(game: Game, faction: Faction) = game.round == 0
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    if (faction == US) {
      game.skipHeadlineCard2 = true
    }
    true
  }
  override def afterPlay(game: Game, faction: Faction): Unit = {
    if (faction == USSR) {
      val currentState = game.stateStack.top
      if (currentState == State.solveHeadLineCard1 || currentState == State.solveHeadLineCard2) {
        return
      }
      if (currentState == State.selectCardAndAction) { // Except space
        return
      }
      game.addVpAndCheck(US, 1)
    }
  }
}
