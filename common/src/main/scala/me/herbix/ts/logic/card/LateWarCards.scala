// Copyright (C) 2017 Chaofan

package me.herbix.ts.logic.card

import me.herbix.ts.logic.Faction.{Faction, _}
import me.herbix.ts.logic.State._
import me.herbix.ts.logic._
import me.herbix.ts.util.ConditionBuilder._
import me.herbix.ts.util.{OperationHint, HistoryRegion, HistoryCardOperation, HistoryEvent}

/**
  * Created by Chaofan on 2016/7/24.
  */
object Card082IranianHostage extends CardInstant(82, 3, USSR, true) {
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    val iran = game.theWorldMap.countries("Iran")
    game.modifyInfluence(US, false, Map(iran -> game.influence(iran, US)))
    game.modifyInfluence(USSR, true, Map(iran -> 2))
    game.addFlag(US, Flags.IranianHostage)
    true
  }
}

object Card083TheIronLady extends CardInstant(83, 3, US, true) {
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    game.addVpAndCheck(US, 1)
    game.modifyInfluence(USSR, true, Map(game.theWorldMap.countries("Argentina") -> 1))
    val uk = game.theWorldMap.countries("UK")
    game.modifyInfluence(USSR, false, Map(uk -> game.influence(uk, USSR)))
    game.addFlag(US, Flags.IronLady)
    true
  }
}

object Card084ReaganBombsLibya extends CardInstant(84, 2, US, true) {
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    val libya = game.theWorldMap.countries("Libya")
    game.addVpAndCheck(US, game.influence(libya, USSR) / 2)
    true
  }
}

object Card085StarWars extends CardMultiStep(85, 2, US, true) {
  override def canEvent(game: Game, faction: Faction) =
    game.space(US).level > game.space(USSR).level &&
    game.discards.exists(c => c.canHeld(game) && c.canEvent(game, faction))

  @prepare
  def showDiscards(game: Game): Unit = {
    game.currentCardData = game.discards
  }

  @step1(cardEventSelectCard, selectCard.canHeld.canEvent)
  def activateEvent(game: Game, input: Operation): Int = {
    game.currentCardData = null
    val card = input.asInstanceOf[OperationSelectCard].card.get

    game.discardsRemove(card)
    game.discardCard(card, US)

    game.currentCard = card
    game.operatingPlayer = card.getOperatingPlayer(faction)
    game.recordHistory(new HistoryEvent(faction, game.currentCard))
    game.stateStack.push(cardEventStart)
    card.nextState(game, faction, null)
    -1
  }
}

object Card086NorthSeaOil extends CardInstant(86, 3, US, true) {
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    game.addFlag(US, Flags.NorthSeaOil)
    game.addFlag(US, Flags.NorthSeaOil8Rounds)
    true
  }
}

object Card087Reformer extends CardMultiStep(87, 3, USSR, true) {
  @step1(cardEventInfluence, (game: Game) => if (game.vp < 0) 6 else 4, true, true, USSR, influence.inRegion(Region.Europe).max(2))
  def addUSSRInfluence(game: Game, input: Operation): Unit = {
    game.modifyInfluence(USSR, true, input.asInstanceOf[OperationModifyInfluence].detail)
    game.addFlag(USSR, Flags.Reformer)
  }
}

object Card088BarracksBombing extends CardMultiStep(88, 2, USSR, true) {
  val checkValidFunc: Map[Country, Int] => Boolean =
    _.forall(e => e._1.regions(Region.MidEast) && e._1.name != "Lebanon")

  @prepare
  def removeLebanonInfluence(game: Game): Unit = {
    val lebanon = game.theWorldMap.countries("Lebanon")
    game.modifyInfluence(US, false, Map(lebanon -> game.influence(lebanon, US)))
  }

  @step1(cardEventInfluence, 2, false, false, US, influence.inRegion(Region.MidEast).notInCountry("Lebanon"))
  def removeUSInfluence(game: Game, input: Operation): Unit = {
    game.modifyInfluence(US, false, input.asInstanceOf[OperationModifyInfluence].detail)
  }
}

object Card089ShootDownKAL007 extends CardMultiStep(89, 4, US, true) {
  @prepare
  def setDefconAndCheckKorea(game: Game): Int = {
    game.setDefcon(game.defcon - 1)
    game.addVpAndCheck(US, 2)
    if (game.getController(game.theWorldMap.countries("S.Korea")) == US) 1 else 4
  }

  @step1(cardEventSpecial)
  def selectOperation(game: Game, input: Operation): Int = {
    val op = input.asInstanceOf[OperationSelectOperation]
    game.recordHistory(new HistoryCardOperation(op.faction, this, op.action))
    game.currentCardData = 4
    if (op.action == Action.Influence) 2 else 3
  }

  @step2(cardEventInfluence, 4, true, true, US, influence.gameCanAddInfluence)
  def operationAddInfluence(game: Game, input: Operation): Int = {
    game.modifyInfluence(US, true, input.asInstanceOf[OperationModifyInfluence].detail)
    game.currentCardData = null
    4
  }

  @step3(cardEventSelectCountry, 1, true, selectCountry.all)
  def operationRealignment(game: Game, input: Operation): Int = {
    if (realignment(game, input)) 4 else 3
  }

  override def getSpecialOperationHint(game: Game): OperationHint =
    OperationHint(classOf[OperationSelectOperation], _ => true, _ => true, _ => false)
}

object Card090Glasnost extends CardMultiStep(90, 4, USSR, true) {
  @prepare
  def setDefconAndCheckReformer(game: Game): Int = {
    game.addVpAndCheck(USSR, 2)
    game.setDefcon(game.defcon + 1)
    if (game.flags.hasFlag(faction, Flags.Reformer)) 1 else 4
  }

  @step1(cardEventSpecial)
  def selectOperation(game: Game, input: Operation): Int = {
    val op = input.asInstanceOf[OperationSelectOperation]
    game.recordHistory(new HistoryCardOperation(op.faction, this, op.action))
    game.currentCardData = 4
    if (op.action == Action.Influence) 2 else 3
  }

  @step2(cardEventInfluence, 4, true, true, US, influence.gameCanAddInfluence)
  def operationAddInfluence(game: Game, input: Operation): Int = {
    game.modifyInfluence(USSR, true, input.asInstanceOf[OperationModifyInfluence].detail)
    game.currentCardData = null
    4
  }

  @step3(cardEventSelectCountry, 1, true, selectCountry.all)
  def operationRealignment(game: Game, input: Operation): Int = {
    if (realignment(game, input)) 4 else 3
  }

  override def getSpecialOperationHint(game: Game): OperationHint =
    OperationHint(classOf[OperationSelectOperation], _ => true, _ => true, _ => false)
}

object Card091OrtegaInNicaragua extends CardMultiStep(91, 2, USSR, true) {
  @prepare
  def removeNicaraguaInfluence(game: Game): Unit = {
    val nicaragua = game.theWorldMap.countries("Nicaragua")
    game.modifyInfluence(US, false, Map(nicaragua -> game.influence(nicaragua, US)))
    game.currentCardData = game.modifyOp(faction, this.op)
  }

  @step1(cardEventSelectCountry, 1, false, selectCountry.withName("Cuba", "Honduras", "Costa Rica").canCoupWithoutFlags)
  def doCoup(game: Game, input: Operation): Unit = {
    val modifier = game.currentCardData.asInstanceOf[Int]
    for (country <- input.asInstanceOf[OperationSelectCountry].detail) {
      game.coup(country, faction, modifier)
    }
    game.currentCardData = null
  }
}

object Card092Terrorism extends CardInstant(92, 2, Neutral, false) {
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    val opposite = Faction.getOpposite(faction)
    val count = if (game.flags.hasFlag(opposite, Flags.IranianHostage)) 2 else 1
    val hand = game.hand(opposite)
    for (i <- 0 until count) {
      if (!hand.isEmptyExcludingChinaCard) {
        val card = game.pickCardFromHand(opposite)
        game.discardCard(card, opposite, true, true)
      }
    }
    true
  }
}

object Card093IranContra extends CardInstant(93, 2, USSR, true) {
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    game.addFlag(US, Flags.IranContra)
    true
  }
}

object Card094Chernobyl extends CardMultiStep(94, 3, US, true) {
  @step1(cardEventSpecial)
  def selectRegion(game: Game, input: Operation): Unit = {
    val region = input.asInstanceOf[OperationSelectRegion].region
    game.recordHistory(new HistoryRegion(faction, region))
    game.addFlag(USSR, Flags.Chernobyl, region)
  }

  override def getSpecialOperationHint(game: Game): OperationHint =
    OperationHint(classOf[OperationSelectRegion])
}

object Card095LatinAmericaDebtCrisis extends CardMultiStep(95, 2, USSR, false) {
  @prepare
  def changeOperatingPlayer(game: Game): Unit = {
    game.operatingPlayerChange(US)
  }

  @step1(cardEventSelectCardOrCancel, selectCard.minModifiedOp(3).canDiscardBy(US))
  def discardUSCard(game: Game, input: Operation): Int = {
    game.operatingPlayerRollBack()
    val op = input.asInstanceOf[OperationSelectCard]
    if (op.card.isDefined) {
      val card = op.card.get
      game.handRemove(US, card)
      game.discardCard(card, US, true, true)
      3
    } else {
      2
    }
  }

  @step2(cardEventSelectCountry, 2, true, selectCountry.inRegion(Region.SouthAmerica))
  def doubleUSSRInfluence(game: Game, input: Operation): Unit = {
    game.modifyInfluence(USSR, true, input.asInstanceOf[OperationSelectCountry].detail.map(country =>
      country -> game.influence(country, USSR)
    ).toMap)
  }
}

object Card096TearDownThisWall extends CardMultiStep(96, 3, US, true) {
  def canCoup(game: Game): Boolean = game.canCoupWithoutFlags(game.playerFaction, c => c.regions(Region.Europe))

  @prepare
  def removeEGermanyInfluence(game: Game): Unit = {
    game.addFlag(US, Flags.TearDownThisWall)
    game.removeFlag(USSR, Flags.WillyBrandt)
    val eGermany = game.theWorldMap.countries("E.Germany")
    game.modifyInfluence(US, true, Map(eGermany -> 3))
  }

  @step1(cardEventSpecial)
  def selectOperation(game: Game, input: Operation): Int = {
    val op = input.asInstanceOf[OperationSelectOperation]
    game.recordHistory(new HistoryCardOperation(op.faction, this, op.action))
    game.currentCardData = game.modifyOp(faction, this.op)
    if (op.action == Action.Realignment) 2 else 3
  }

  @step2(cardEventSelectCountry, 1, false, selectCountry.inRegion(Region.Europe))
  def doRealignment(game: Game, input: Operation): Int = {
    if (realignment(game, input)) 4 else 2
  }

  @step3(cardEventSelectCountry, 1, false, selectCountry.inRegion(Region.Europe).canCoupWithoutFlags)
  def doCoup(game: Game, input: Operation): Unit = {
    val modifier = game.currentCardData.asInstanceOf[Int]
    for (country <- input.asInstanceOf[OperationSelectCountry].detail) {
      game.coup(country, faction, modifier)
    }
    game.currentCardData = null
  }

  override def getSpecialOperationHint(game: Game): OperationHint =
    OperationHint(classOf[OperationSelectOperation], _ => false, _ => true, canCoup)
}

object Card097EvilEmpire extends CardInstant(97, 3, US, true) {
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    game.addVpAndCheck(US, 1)
    game.removeFlag(US, Flags.FlowerPower)
    game.addFlag(US, Flags.EvilEmpire)
    true
  }
}

object Card098AldrichAmes extends CardMultiStep(98, 3, USSR, true) {
  @prepare
  def checkHasCards(game: Game): Int = {
    game.addFlag(US, Flags.AldrichAmes)
    if (game.hand(US).cardCountExcludingChinaCard == 0) {
      return 2
    }
    val eventCards = new CardSet(game)
    game.hand(US).iteratorExcludingChinaCard.foreach(eventCards.add)
    game.currentCardData = eventCards
    game.clearSnapshots()
    1
  }

  @step1(cardEventSelectCard, selectCard.all)
  def discardUSCard(game: Game, input: Operation): Unit = {
    game.currentCardData = null
    val card = input.asInstanceOf[OperationSelectCard].card.get
    game.handRemove(US, card)
    game.discardCard(card, US, true, true)
  }
}

object Card099PershingII extends CardMultiStep(99, 3, USSR, true) {
  @prepare
  def addVp(game: Game): Unit = {
    game.addVpAndCheck(USSR, 1)
  }

  @step1(cardEventInfluence, 3, false, false, US, influence.inRegion(Region.WestEurope).max(1))
  def removeUSInfluence(game: Game, input: Operation): Unit = {
    game.modifyInfluence(US, false, input.asInstanceOf[OperationModifyInfluence].detail)
  }
}

object Card100WarGames extends CardInstant(100, 4, Neutral, true) {
  override def canEvent(game: Game, faction: Faction) = game.defcon == 2
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    game.addVpAndCheck(Faction.getOpposite(faction), 6)
    game.endGameByVp()
    true
  }
}

object Card101Solidarity extends CardInstant(101, 2, US, true) {
  override def canEvent(game: Game, faction: Faction) = game.flags.hasFlag(Flags.JohnPaulII)
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    val poland = game.theWorldMap.countries("Poland")
    game.modifyInfluence(US, true, Map(poland -> 3))
    true
  }
}

object Card102IranIraqWar extends CardMultiStep(102, 2, Neutral, false) {
  @step1(cardEventSelectCountry, 1, true, selectCountry.withName("Iran", "Iraq"))
  def doWar(game: Game, input: Operation): Unit = {
    val country = input.asInstanceOf[OperationSelectCountry].detail.head
    val opposite = Faction.getOpposite(faction)
    val modifier = country.adjacentCountries.count(game.getController(_) == opposite)
    game.war(faction, country, modifier, 4, 2, 2)
  }

  override def afterPlay(game: Game, faction: Faction): Unit = {
    Card059FlowerPower.triggerFlowerPowerEffect(game, faction)
  }
}
