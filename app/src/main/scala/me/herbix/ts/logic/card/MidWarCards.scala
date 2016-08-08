package me.herbix.ts.logic.card

import me.herbix.ts.logic.Faction.{Faction, _}
import me.herbix.ts.logic.Region.RegionState
import me.herbix.ts.logic.State._
import me.herbix.ts.logic._
import me.herbix.ts.util.ConditionBuilder._

/**
  * Created by Chaofan on 2016/7/24.
  */
object Card036BrushWar extends CardMultiStep(36, 3, Neutral, false) {
  val NATOCondition: (Game, Set[Country]) => Boolean = (game, detail) =>
    !game.flags.hasFlag(game.operatingPlayer, Flags.NATO) ||
    detail.forall(c => !(c.getController(game) == Faction.getOpposite(game.operatingPlayer) && c.regions(Region.Europe)))

  @step1(cardEventSelectCountry, 1, true, selectCountry.maxStability(2).extraCondition(NATOCondition))
  def doWar(game: Game, input: Operation): Unit = {
    val country = input.asInstanceOf[OperationSelectCountry].detail.head
    val opposite = Faction.getOpposite(faction)
    val modifier = country.adjacentCountries.count(_.getController(game) == opposite)
    game.war(faction, country, modifier, 3, 3, 1)
  }

  override def afterPlay(game: Game, faction: Faction): Unit = {
    Card059FlowerPower.triggerFlowerPowerEffect(game, faction)
  }
}

object Card037MidAmericaScoring extends CardScoring(37, Region.MidAmerica)

object Card038SEAsiaScoring extends CardInstant(38, 0, Neutral, true) {
  override def canHeld(game: Game) = false
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    val targetCountries = WorldMap.countries.values.filter(_.regions(Region.SouthEastAsia))
    val thailand = WorldMap.countries("Thailand")
    val usVp = targetCountries.count(_.getController(game) == US) + (if (thailand.getController(game) == US) 1 else 0)
    val ussrVp = targetCountries.count(_.getController(game) == USSR) + (if (thailand.getController(game) == USSR) 1 else 0)
    game.addVp(US, usVp)
    game.addVp(USSR, ussrVp)
    game.checkVp()
    true
  }
}

object Card039ArmsRace extends CardInstant(39, 3, Neutral, false) {
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    val military1 = game.military(faction)
    val military2 = game.military(Faction.getOpposite(faction))
    if (military1 > military2) {
      var vp = 1
      if (military1 >= game.defcon) {
        vp = 3
      }
      game.addVpAndCheck(faction, vp)
    }
    true
  }
}

object Card040CubaMissile extends CardInstant(40, 3, Neutral, true) {
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    game.setDefcon(2)
    game.addFlag(Faction.getOpposite(faction), Flags.CubaMissile)
    true
  }
  def getConditionByFaction(faction: Faction): (Game, Set[Country]) => Boolean = {
    if (faction == Faction.US)
      (game, detail) => detail.forall(c => (c.name == "Turkey" || c.name == "W.Germany") && game.influence(c, faction) >= 2)
    else
      (game, detail) => detail.forall(c => c.name == "Cuba" && game.influence(c, faction) >= 2)
  }
}

object Card041NuclearSubs extends CardInstant(41, 2, US, true) {
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    game.addFlag(US, Flags.NuclearSubs)
    true
  }
}

object Card042Quagmire extends CardInstant(42, 3, USSR, true) {
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    game.addFlag(US, Flags.QuagmireBearTrap)
    game.removeFlag(US, Flags.NORAD)
    true
  }
}

object Card043SALTNegotiations extends CardMultiStep(43, 3, Neutral, true) {
  @prepare
  def prepare(game: Game): Int = {
    game.setDefcon(game.defcon + 2)
    game.addFlag(Neutral, Flags.SALT)
    if (game.discards.isEmpty) return 2
    game.currentCardData = game.discards
    1
  }

  @step1(cardEventSelectCardOrCancel, selectCard.canHeld)
  def acquireCardFromDiscards(game: Game, input: Operation): Unit = {
    game.currentCardData = null
    for (card <- input.asInstanceOf[OperationSelectCard].card) {
      game.recordHistory(new HistoryGetCard(faction, card))
      game.handAdd(faction, card)
      game.discardsRemove(card)
    }
  }
}

object Card044BearTrap extends CardInstant(44, 3, US, true) {
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    game.addFlag(USSR, Flags.QuagmireBearTrap)
    true
  }
}

object Card045Summit extends CardMultiStep(45, 1, Neutral, false) {
  @prepare
  def rollDice(game: Game): Int = {
    val usRoll = game.rollDice()
    val ussrRoll = game.rollDice()
    val counters = Region.MainRegionSet.map(Region.getRegionState(game, _))
    val usModifier = counters.toStream.map(_(US)).count(s => s == RegionState.Domination || s == RegionState.Control)
    val ussrModifier = counters.toStream.map(_(USSR)).count(s => s == RegionState.Domination || s == RegionState.Control)
    val usRollResult = usRoll + usModifier
    val ussrRollResult = ussrRoll + ussrModifier

    game.recordHistory(new HistoryRollDice(US, usRoll, usModifier))
    game.recordHistory(new HistoryRollDice(USSR, ussrRoll, ussrModifier))

    val targetFaction = if (usRollResult > ussrRollResult) US else if (usRollResult < ussrRollResult) USSR else Neutral
    if (targetFaction == Neutral) {
      2
    } else {
      game.operatingPlayer = targetFaction
      game.addVpAndCheck(targetFaction, 2)
      1
    }
  }

  @step1(cardEventSpecial)
  def setDefcon(game: Game, input: Operation): Unit = {
    game.setDefcon(game.defcon + input.asInstanceOf[OperationIntValue].value)
  }

  override def getSpecialOperationHint(game: Game): OperationHint = OperationHint(classOf[OperationIntValue], -1, 1)
}

object Card046HowILearnStopWorry extends CardMultiStep(46, 2, Neutral, true) {
  @step1(cardEventSpecial)
  def setDefconAndMilitary(game: Game, input: Operation): Unit = {
    game.setDefcon(input.asInstanceOf[OperationIntValue].value)
    game.addMilitary(faction, 5)
  }

  override def getSpecialOperationHint(game: Game): OperationHint = OperationHint(classOf[OperationIntValue], 1, 5)
}

object Card047Junta extends CardMultiStep(47, 2, Neutral, false) {
  def canCoup(game: Game): Boolean = game.canCoupWithoutFlags(game.playerFaction,
    c => c.regions(Region.MidAmerica) || c.regions(Region.SouthAmerica))

  @step1(cardEventSelectCountry, 1, true, selectCountry.inRegion(Region.MidAmerica, Region.SouthAmerica))
  def addInfluence(game: Game, input: Operation): Unit = {
    val country = input.asInstanceOf[OperationSelectCountry].detail.head
    game.modifyInfluence(faction, true, Map(country -> 2))
  }

  @step2(cardEventSpecial)
  def selectOperation(game: Game, input: Operation): Int = {
    val op = input.asInstanceOf[OperationSelectOperation]
    game.recordHistory(new HistoryCardOperation(op.faction, this, op.action))
    game.currentCardData = game.modifyOp(faction, this.op)
    if (op.action == Action.Realignment) 3 else 4
  }

  @step3(cardEventSelectCountry, 1, false, selectCountry.inRegion(Region.MidAmerica, Region.SouthAmerica))
  def doRealignment(game: Game, input: Operation): Int = {
    if (realignment(game, input)) 5 else 3
  }

  @step4(cardEventSelectCountry, 1, false, selectCountry.inRegion(Region.MidAmerica, Region.SouthAmerica).canCoupWithoutFlags)
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

object Card048KitchenDebates extends CardInstant(48, 1, US, true) {
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    val targetCountries = WorldMap.countries.values
    val usCount = targetCountries.count(country => country.isBattlefield && country.getController(game) == US)
    val ussrCount = targetCountries.count(country => country.isBattlefield && country.getController(game) == USSR)
    if (usCount > ussrCount) {
      game.pokeChest(US)
      game.addVpAndCheck(US, 2)
    }
    true
  }
}

object Card049MissileEnvy extends CardMultiStep(49, 2, Neutral, true) {
  override def canEvent(game: Game, faction: Faction): Boolean =
    !game.flags.hasFlag(faction, Flags.MissileEnvy) && !game.hand(Faction.getOpposite(faction)).isEmptyExcludingChinaCard
  class Card049MissileEnvyDummy(tmpop: Int) extends Card(49, 2, Neutral, false) {
    override val op = tmpop
    override def modifyOp(game: Game, faction: Faction, originalOp: Int): Int = op
    override def nextState(game: Game, faction: Faction, input: Operation): Unit =
      Card049MissileEnvy.nextState(game, faction, input)
  }
  object Card049MissileEnvyDummy {
    val map = (0 to 4).map(i => i -> new Card049MissileEnvyDummy(i)).toMap
    def apply(op: Int) = map(op)
  }

  @prepare
  def prepareExchangeCards(game: Game): Unit = {
    val opposite = Faction.getOpposite(faction)
    game.currentCardData = game.hand(opposite).iteratorExcludingChinaCard.map(card => game.modifyOp(opposite, card.op)).max
    game.operatingPlayerChange(opposite)
  }

  @step1(cardEventSelectCard, selectCard.canDiscard.minModifiedOpLargerThanCardData)
  def exchangeCard(game: Game, input: Operation): Int = {
    val opposite = game.operatingPlayer
    game.operatingPlayerRollBack()

    val faction = game.operatingPlayer
    val card = input.asInstanceOf[OperationSelectCard].card.get

    game.handRemove(opposite, card)
    game.handAdd(opposite, this)

    game.recordHistory(new HistoryLoseCard(opposite, card))
    game.recordHistory(new HistoryGetCard(opposite, this))

    game.addFlag(opposite, Flags.MissileEnvy)

    val activateEvent = card.faction != opposite && card.canEvent(game, faction)
    game.discardCard(card, opposite, !activateEvent)
    if (card.faction != opposite) {
      if (card.canEvent(game, faction)) {
        game.currentCard = card
        game.operatingPlayer = faction
        game.recordHistory(new HistoryEvent(faction, game.currentCard))
        game.stateStack.push(cardEventStart)
        card.nextState(game, faction, null)
        -1
      } else {
        3
      }
    } else {
      game.currentCardChange(Card049MissileEnvyDummy(card.op))
      2
    }
  }

  @step2(cardEventOperation)
  def operationDone(game: Game): Unit = {
    game.currentCardRollBack()
  }

  override def afterPlay(game: Game, faction: Faction): Unit = {
    if (game.flags.hasFlag(faction, Flags.MissileEnvy)) {
      game.removeFlag(faction, Flags.MissileEnvy)
    }
  }
}

object Card050WeWillBuryYou extends CardInstant(50, 4, USSR, true) {
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    game.addFlag(US, Flags.WeWillBuryYou)
    game.setDefcon(game.defcon - 1)
    true
  }
}

object Card051BrezhnevDoctrine extends CardInstant(51, 3, USSR, true) {
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    game.addFlag(USSR, Flags.Containment)
    true
  }
}

object Card052PortugueseCrumbles extends CardInstant(52, 2, USSR, true) {
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    val seAfricanStates = WorldMap.countries("SE African States")
    val angola = WorldMap.countries("Angola")
    game.modifyInfluence(USSR, true, Map(seAfricanStates -> 2, angola -> 2))
    true
  }
}

object Card053SouthAfricaUnrest extends CardMultiStep(53, 2, USSR, false) {
  val countries = Set("Angola", "Botswana", "South Africa")
  val checkValidFunc: Set[Country] => Boolean =
    set => set.forall(c => countries(c.name)) && !(set.size == 2 && !set.exists(_.name == "South Africa"))

  @step1(cardEventSelectCountry, 2, false, checkValidFunc)
  def addUSSRInfluence(game: Game, input: Operation): Unit = {
    val set = input.asInstanceOf[OperationSelectCountry].detail
    val southAfrica = WorldMap.countries("South Africa")
    val target2 = if (set.size == 1) {
      if (set.head == southAfrica) null else set.head
    } else {
      (set - southAfrica).head
    }
    if (target2 == null) {
      game.modifyInfluence(USSR, true, Map(southAfrica -> 2))
    } else {
      game.modifyInfluence(USSR, true, Map(southAfrica -> 1, target2 -> 2))
    }
  }
}

object Card054Allende extends CardInstant(54, 1, USSR, true) {
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    val chile = WorldMap.countries("Chile")
    game.modifyInfluence(USSR, true, Map(chile -> 2))
    true
  }
}

object Card055WillyBrandt extends CardInstant(55, 2, USSR, true) {
  override def canEvent(game: Game, faction: Faction): Boolean = !game.flags.hasFlag(Flags.TearDownThisWall)
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    val wGermany = WorldMap.countries("W.Germany")
    game.addVpAndCheck(USSR, 1)
    game.modifyInfluence(USSR, true, Map(wGermany -> 1))
    game.addFlag(USSR, Flags.WillyBrandt)
    true
  }
}

object Card056MuslimRevolution extends CardMultiStep(56, 4, USSR, false) {
  val countryNames = Set("Sudan", "Iran", "Iraq", "Egypt", "Libya", "Saudi Arabia", "Syria", "Jordan")

  override def canEvent(game: Game, faction: Faction) = !game.flags.hasFlag(Flags.AwacsSale)

  @step1(cardEventSelectCountry, 2, true, selectCountry.withName(countryNames))
  def removeUSInfluence(game: Game, input: Operation): Unit = {
    game.modifyInfluence(US, false, input.asInstanceOf[OperationSelectCountry].detail.map(country =>
      country -> game.influence(country, US)
    ).toMap)
  }
}

object Card057ABMTreaty extends CardMultiStep(57, 4, Neutral, false) {
  override def modifyOp(game: Game, faction: Faction, originalOp: Int): Int = {
    if (game.currentCardData == true) op else originalOp
  }

  @prepare
  def setDefcon(game: Game): Unit = {
    game.setDefcon(game.defcon + 1)
    game.currentCardData = true
  }

  @step1(cardEventOperation)
  def operationDone(game: Game): Unit = {
    game.currentCardData = null
  }
}

object Card058CulturalRevolution extends CardInstant(58, 3, USSR, true) {
  override def canEvent(game: Game, faction: Faction): Boolean = !game.flags.hasFlag(Flags.ChineseCivilWar)
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    val chinaCard = Cards.chinaCard
    if (game.hand(US).has(chinaCard)) {
      game.handRemove(US, chinaCard)
      game.handAdd(USSR, chinaCard)
      game.flags.removeFlag(US, Flags.CantPlayChinaCard)
      game.flags.removeFlag(USSR, Flags.CantPlayChinaCard)
      game.recordHistory(new HistoryLoseCard(US, chinaCard))
      game.recordHistory(new HistoryGetCard(USSR, chinaCard))
    } else {
      game.addVpAndCheck(USSR, 1)
    }
    true
  }
}

object Card059FlowerPower extends CardInstant(59, 4, USSR, true) {
  override def canEvent(game: Game, faction: Faction) = !game.flags.hasFlag(Flags.EvilEmpire)
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    game.addFlag(US, Flags.FlowerPower)
    true
  }
  def triggerFlowerPowerEffect(game: Game, faction: Faction): Unit = {
    if (game.stateStack.top == State.selectCardAndAction) { // Except space
      return
    }
    if (game.flags.hasFlag(faction, Flags.FlowerPower)) {
      game.addVpAndCheck(Faction.getOpposite(faction), 2)
    }
  }
}

object Card060U2Incident extends CardInstant(60, 3, USSR, true) {
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    game.addVpAndCheck(USSR, 1)
    game.addFlag(USSR, Flags.U2Incident)
    true
  }
}

object Card061Opec extends CardInstant(61, 3, USSR, false) {
  val countries = Set("Egypt", "Iran", "Libya", "Saudi Arabia", "Iraq", "Gulf States", "Venezuela")
  override def canEvent(game: Game, faction: Faction) = !game.flags.hasFlag(Flags.NorthSeaOil)
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    val vp = countries.count(WorldMap.countries(_).getController(game) == USSR)
    game.addVpAndCheck(USSR, vp)
    true
  }
}

object Card062LoneGunman extends CardMultiStep(62, 1, USSR, true) {
  override def modifyOp(game: Game, faction: Faction, originalOp: Int): Int = {
    if (game.currentCardData == true) op else originalOp
  }

  @prepare
  def prepareShowHand(game: Game): Unit = {
    game.currentCardData = game.hand(US)
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

object Card063ColonialRearGuards extends CardMultiStep(63, 2, US, false) {
  @step1(cardEventInfluence, 4, true, true, US, influence.inRegion(Region.Africa, Region.SouthEastAsia).max(1))
  def addUSInfluence(game: Game, input: Operation): Unit = {
    game.modifyInfluence(US, true, input.asInstanceOf[OperationModifyInfluence].detail)
  }
}

object Card064PanamaCanalReturned extends CardInstant(64, 1, US, true) {
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    game.modifyInfluence(US, true, Map(
      WorldMap.countries("Panama") -> 1,
      WorldMap.countries("Venezuela") -> 1,
      WorldMap.countries("Costa Rica") -> 1
    ))
    true
  }
}

object Card065CampDavidAccords extends CardInstant(65, 2, US, true) {
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    game.addVpAndCheck(US, 1)
    game.modifyInfluence(US, true, Map(
      WorldMap.countries("Israel") -> 1,
      WorldMap.countries("Jordan") -> 1,
      WorldMap.countries("Egypt") -> 1
    ))
    game.addFlag(US, Flags.CampDavid)
    true
  }
}

object Card066PuppetGovernments extends CardMultiStep(66, 2, US, true) {
  @step1(cardEventInfluence, 3, true, false, US, influence.noInfluence.max(1))
  def addUSInfluence(game: Game, input: Operation): Unit = {
    game.modifyInfluence(US, true, input.asInstanceOf[OperationModifyInfluence].detail)
  }
}

object Card067GrainSales extends CardMultiStep(67, 2, US, false) {
  @prepare
  def randomPickCard(game: Game): Int = {
    if (game.hand(USSR).cardCountExcludingChinaCard > 0) {
      val card = game.hand(USSR).pick(game.random)
      game.currentCardData = card
      1
    } else {
      2
    }
  }

  @step1(cardEventSpecial)
  def playOrReturn(game: Game, input: Operation): Int = {
    if (input.asInstanceOf[OperationYesNo].value) {
      val card = game.currentCardData.asInstanceOf[Card]
      game.handRemove(USSR, card)
      game.recordHistory(new HistoryLoseCard(USSR, card))
      game.currentCardChange(card)
      game.currentCardData = null
      3
    } else {
      game.currentCardData = null
      2
    }
  }

  @step2(cardEventOperation)
  def operationDone(): Int = 4

  @step3(cardEventAnotherCard)
  def playCardDone(): Unit = {}

  override def getSpecialOperationHint(game: Game): OperationHint = OperationHint(classOf[OperationYesNo], false)
}

object Card068JohnPaulII extends CardInstant(68, 2, US, true) {
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    val poland = WorldMap.countries("Poland")
    game.modifyInfluence(USSR, false, Map(poland -> 2))
    game.modifyInfluence(US, true, Map(poland -> 1))
    game.addFlag(US, Flags.JohnPaulII)
    true
  }
}

object Card069LatinAmericaSquads extends CardInstant(69, 2, Neutral, false) {
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    game.addFlag(faction, Flags.DeathSquads)
    game.addFlag(Faction.getOpposite(faction), Flags.DeathSquads2)
    true
  }
}

object Card070OASFounded extends CardMultiStep(70, 1, US, true) {
  @step1(cardEventInfluence, 2, true, true, US, influence.inRegion(Region.MidAmerica, Region.SouthAmerica))
  def addUSInfluence(game: Game, input: Operation): Unit = {
    game.modifyInfluence(US, true, input.asInstanceOf[OperationModifyInfluence].detail)
  }
}

object Card071NixonPlaysTheChinaCard extends CardInstant(71, 2, US, true) {
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    val chinaCard = Cards.chinaCard
    if (game.hand(USSR).has(chinaCard)) {
      game.handRemove(USSR, chinaCard)
      game.handAdd(US, chinaCard)
      game.flags.removeFlag(USSR, Flags.CantPlayChinaCard)
      game.flags.addFlag(US, Flags.CantPlayChinaCard)
      game.recordHistory(new HistoryLoseCard(USSR, chinaCard))
      game.recordHistory(new HistoryGetCard(US, chinaCard))
    } else {
      game.addVpAndCheck(US, 2)
    }
    true
  }
}

object Card072SadatExpelsSoviets extends CardInstant(72, 1, US, true) {
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    val egypt = WorldMap.countries("Egypt")
    game.modifyInfluence(USSR, false, Map(egypt -> game.influence(egypt, USSR)))
    game.modifyInfluence(US, true, Map(egypt -> 1))
    true
  }
}

object Card073ShuttleDiplomacy extends CardInstant(73, 3, US, true) {
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    game.addFlag(US, Flags.ShuttleDiplomacy)
    true
  }
}

object Card074VoiceOfAmerica extends CardMultiStep(74, 2, US, false) {
  @step1(cardEventInfluence, 4, false, false, USSR, influence.notInRegion(Region.Europe).max(2))
  def removeUSSRInfluence(game: Game, input: Operation): Unit = {
    game.modifyInfluence(USSR, false, input.asInstanceOf[OperationModifyInfluence].detail)
  }
}

object Card075LiberationTheology extends CardMultiStep(75, 2, USSR, false) {
  @step1(cardEventInfluence, 3, true, true, USSR, influence.inRegion(Region.MidAmerica).max(2))
  def addUSSRInfluence(game: Game, input: Operation): Unit = {
    game.modifyInfluence(USSR, true, input.asInstanceOf[OperationModifyInfluence].detail)
  }
}

object Card076UssuriRiverSkirmish extends CardMultiStep(76, 3, US, true) {
  @prepare
  def checkChinaCard(game: Game): Int = {
    val chinaCard = Cards.chinaCard
    if (game.hand(USSR).has(chinaCard)) {
      game.handRemove(USSR, chinaCard)
      game.handAdd(US, chinaCard)
      game.flags.removeFlag(USSR, Flags.CantPlayChinaCard)
      game.flags.removeFlag(US, Flags.CantPlayChinaCard)
      game.recordHistory(new HistoryLoseCard(USSR, chinaCard))
      game.recordHistory(new HistoryGetCard(US, chinaCard))
      2
    } else {
      1
    }
  }
  @step1(cardEventInfluence, 4, true, true, US, influence.inRegion(Region.Asia).max(2))
  def addUSInfluence(game: Game, input: Operation): Unit = {
    game.modifyInfluence(US, true, input.asInstanceOf[OperationModifyInfluence].detail)
  }
}

object Card077AskNotWhatYourCountry extends CardMultiStep(77, 3, US, true) {
  @step1(cardEventSelectMultipleCards, selectCard.canDiscard)
  def discardCards(game: Game, input: Operation): Unit = {
    val cards = input.asInstanceOf[OperationSelectCards].cards
    for (card <- cards) {
      game.handRemove(US, card)
      game.discardCard(card, US, true, true)
    }
    for (i <- 0 until cards.size) {
      game.handAdd(US, game.pickCardFromDeck())
    }
    game.recordHistory(new HistoryPickCard(US, cards.size))
  }
}

object Card078AllianceForProgress extends CardInstant(78, 3, US, true) {
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    val vp = WorldMap.countries.values.count(country => {
      country.isBattlefield &&
        (country.regions(Region.MidAmerica) || country.regions(Region.SouthAmerica)) &&
        country.getController(game) == US
    })
    game.addVpAndCheck(US, vp)
    true
  }
}

object Card079AfricaScoring extends CardScoring(79, Region.Africa)

object Card080SmallStep extends CardInstant(80, 2, Neutral, false) {
  override def canEvent(game: Game, faction: Faction) =
    game.space(faction).level < game.space(Faction.getOpposite(faction)).level
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    game.increaseSpace(faction, 2)
    true
  }
}

object Card081SouthAmericaScoring extends CardScoring(81, Region.SouthAmerica)
