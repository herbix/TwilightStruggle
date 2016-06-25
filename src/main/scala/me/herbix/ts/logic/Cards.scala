package me.herbix.ts.logic

import me.herbix.ts.logic.Faction._
import me.herbix.ts.logic.Region.Region

import scala.collection.mutable

/**
  * Created by Chaofan on 2016/6/17.
  */
abstract class Card(val id: Int, val op: Int, val faction: Faction, val isRemovedAfterEvent: Boolean) {
  def canHeadline(game: Game, faction: Faction) = canEvent(game, faction)
  def canDiscard(game: Game, faction: Faction): Boolean = {
    if (game.flags.hasFlag(faction, Flags.QuagmireBearTrap)) {
      return canHeld(game)
    }
    true
  }
  def canEvent(game: Game, faction: Faction) = true
  def canPlay(game: Game, faction: Faction): Boolean = {
    if (game.flags.hasFlag(faction, Flags.QuagmireBearTrap)) {
      return !canHeld(game) || (game.modifyOp(faction, op) >= 2 && canDiscard(game, faction))
    }
    true
  }
  def canHeld(game: Game) = true
  def afterPlay(game: Game, faction: Faction): Unit = {}
  def instantEvent(game: Game, faction: Faction): Boolean = true
  def modifyOp(faction: Faction, originalOp: Int, targets: Iterable[Country]): Int = originalOp
  def modifyOp(faction: Faction, originalOp: Int): Int = originalOp
  def nextState(game: Game, faction: Faction, input: Operation): Unit = {
    if (game.stateStack.top == State.cardEventStart) {
      if (instantEvent(game, faction)) {
        game.stateStack.pop()
        game.stateStack.push(State.cardEventEnd)
      }
    }
  }
}

private object CardUnknown extends Card(0, 0, Neutral, false)

private class CardScoring(id: Int, val region: Region, val presence: Int, val domination: Int, val control: Int)
  extends Card(id, 0, Neutral, false) {
  override def canHeld(game: Game) = false
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    game.scoring(region, presence, domination, control)
    game.checkVp()
    true
  }
}

private object Card004DuckNCover extends Card(4, 3, US, false) {
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    game.setDefcon(game.defcon - 1)
    game.addVpAndCheck(US, 5 - game.defcon)
    true
  }
}

private object Card005FiveYearPlan extends Card(5, 3, US, false) {
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    val ussrHand = game.hand(USSR)
    if (ussrHand.isEmptyExcludingChinaCard) {
      return true
    }
    val card = ussrHand.pickAndRemove(game.random)
    val activateEvent = card.faction == US
    game.discardCard(card, USSR, !activateEvent, true)
    if (activateEvent) {
      game.currentCard = card
      game.recordHistory(new HistoryEvent(US, game.currentCard))
      card.nextState(game, US, null)
      false
    } else {
      true
    }
  }
}

private object Card006ChinaCard extends Card(6, 4, Neutral, false) {
  override def canEvent(game: Game, faction: Faction) = false
  override def canPlay(game: Game, faction: Faction) =
    super.canPlay(game, faction) && !game.flags.hasFlag(faction, Flags.CantPlayChinaCard)
  override def canDiscard(game: Game, faction: Faction) = false
  override def modifyOp(faction: Faction, originalOp: Int, targets: Iterable[Country]): Int =
    if (targets.forall(_.regions.contains(Region.Asia))) {
      originalOp + 1
    } else {
      originalOp
    }
  override def afterPlay(game: Game, faction: Faction): Unit = {
    if (faction == US) {
      game.removeFlag(US, Flags.Taiwan)
    }
  }
}

private object Card008Fidel extends Card(8, 2, USSR, true) {
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    val cuba = game.worldMap.countries("Cuba")
    game.modifyInfluence(US, false, Map(cuba -> cuba.influence(US)))
    game.modifyInfluence(USSR, true, Map(cuba -> cuba.stability))
    true
  }
}

private object Card009VietnamRevolts extends Card(9, 2, USSR, true) {
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    val vietnam = game.worldMap.countries("Vietnam")
    game.modifyInfluence(USSR, true, Map(vietnam -> 2))
    game.addFlag(USSR, Flags.VietnamRevolts)
    true
  }
}

private object Card011KoreanWar extends Card(11, 2, USSR, true) {
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    val southKorea = game.worldMap.countries("S.Korea")
    val modifier = game.worldMap.links(southKorea.name).count(game.worldMap.countries(_).getController == US)
    game.war(USSR, southKorea, modifier, 4, 2, 2)
    true
  }
  override def afterPlay(game: Game, faction: Faction): Unit = {
    Card059FlowerPower.triggerFlowerPowerEffect(game, faction)
  }
}

private object Card012RomanianAbdication extends Card(12, 1, USSR, true) {
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    val romania = game.worldMap.countries("Romania")
    game.modifyInfluence(US, false, Map(romania -> romania.influence(US)))
    game.modifyInfluence(USSR, true, Map(romania -> romania.stability))
    true
  }
}

private object Card013ArabIsraeliWar extends Card(13, 2, USSR, false) {
  override def canEvent(game: Game, faction: Faction) = !game.flags.hasFlag(Flags.CampDavid)
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    val israel = game.worldMap.countries("Israel")
    val modifier = game.worldMap.links(israel.name).count(game.worldMap.countries(_).getController == US) +
      (if (israel.getController == US) 1 else 0)
    game.war(USSR, israel, modifier, 4, 2, 2)
    true
  }
  override def afterPlay(game: Game, faction: Faction): Unit = {
    Card059FlowerPower.triggerFlowerPowerEffect(game, faction)
  }
}

private object Card015Nasser extends Card(15, 1, USSR, true) {
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    val egypt = game.worldMap.countries("Egypt")
    game.modifyInfluence(US, false, Map(egypt -> (egypt.influence(US) + 1) / 2))
    game.modifyInfluence(USSR, true, Map(egypt -> 2))
    true
  }
}

private object Card017DeGaulle extends Card(17, 3, USSR, true) {
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    val france = game.worldMap.countries("France")
    game.modifyInfluence(US, false, Map(france -> 2))
    game.modifyInfluence(USSR, true, Map(france -> 1))
    game.addFlag(USSR, Flags.DeGaulle)
    true
  }
}

private object Card018CaptureNazi extends Card(18, 1, Neutral, true) {
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    game.increaseSpace(faction, 1)
    true
  }
}

private object Card025Containment extends Card(25, 3, US, true) {
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    game.addFlag(US, Flags.Containment)
    true
  }
}

private object Card027USJapanPact extends Card(27, 4, US, true) {
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    val japan = game.worldMap.countries("Japan")
    val change = japan.influence(USSR) + japan.stability - japan.influence(US)
    if (change > 0) {
      game.modifyInfluence(US, true, Map(japan -> change))
    }
    game.addFlag(USSR, Flags.USJapanPact)
    true
  }
}

private object Card031RedScarePurge extends Card(31, 4, Neutral, false) {
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    game.addFlag(Faction.getOpposite(faction), Flags.RedScarePurge)
    true
  }
}

private object Card034NuclearTestBan extends Card(34, 4, Neutral, false) {
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    game.addVpAndCheck(faction, game.defcon - 2)
    game.setDefcon(game.defcon + 2)
    true
  }
}

private object Card035Taiwan extends Card(35, 2, US, true) {
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    game.addFlag(Faction.US, Flags.Taiwan)
    true
  }
}

private object Card038SEAsiaScoring extends Card(38, 0, Neutral, true) {
  override def canHeld(game: Game) = false
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    val targetCountries = game.worldMap.countries.values.filter(_.regions.contains(Region.SouthEastAsia))
    val thailand = game.worldMap.countries("Thailand")
    val usVp = targetCountries.count(_.getController == US) + (if (thailand.getController == US) 1 else 0)
    val ussrVp = targetCountries.count(_.getController == USSR) + (if (thailand.getController == USSR) 1 else 0)
    game.addVp(US, usVp)
    game.addVp(USSR, ussrVp)
    game.checkVp()
    true
  }
}

private object Card039ArmsRace extends Card(39, 3, Neutral, false) {
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

private object Card040CubaMissile extends Card(40, 3, Neutral, true) {
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    game.setDefcon(2)
    game.addFlag(Faction.getOpposite(faction), Flags.CubaMissile)
    true
  }
}

private object Card041NuclearSubs extends Card(41, 2, US, true) {
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    game.addFlag(US, Flags.NuclearSubs)
    true
  }
}

private object Card042Quagmire extends Card(42, 3, USSR, true) {
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    game.addFlag(US, Flags.QuagmireBearTrap)
    true
  }
}

private object Card044BearTrap extends Card(44, 3, US, true) {
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    game.addFlag(US, Flags.QuagmireBearTrap)
    true
  }
}

private object Card048KitchenDebates extends Card(48, 1, US, true) {
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    val targetCountries = game.worldMap.countries.values
    val usCount = targetCountries.count(country => country.isBattlefield && country.getController == US)
    val ussrCount = targetCountries.count(country => country.isBattlefield && country.getController == USSR)
    if (usCount > ussrCount) {
      game.pokeChest(US)
      game.addVpAndCheck(US, 2)
    }
    true
  }
}

private object Card050WeWillBuryYou extends Card(50, 4, USSR, true) {
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    game.addFlag(US, Flags.WeWillBuryYou)
    game.setDefcon(game.defcon - 1)
    true
  }
}

private object Card051BrezhnevDoctrine extends Card(51, 3, USSR, true) {
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    game.addFlag(USSR, Flags.Containment)
    true
  }
}

private object Card054Allende extends Card(54, 1, USSR, true) {
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    val chile = game.worldMap.countries("Chile")
    game.modifyInfluence(USSR, true, Map(chile -> 2))
    true
  }
}

private object Card055WillyBrandt extends Card(55, 2, USSR, true) {
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    val wGermany = game.worldMap.countries("W.Germany")
    game.addVpAndCheck(USSR, 1)
    game.modifyInfluence(USSR, true, Map(wGermany -> 1))
    game.addFlag(USSR, Flags.WillyBrandt)
    true
  }
}

private object Card058CulturalRevolution extends Card(58, 3, USSR, true) {
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    val chinaCard = Cards.chinaCard
    if (game.hand(US).has(chinaCard)) {
      game.hand(US).remove(chinaCard)
      game.discardCard(chinaCard, US, true, true)
      game.flags.removeFlag(US, Flags.CantPlayChinaCard)
      game.flags.removeFlag(USSR, Flags.CantPlayChinaCard)
    } else {
      game.addVpAndCheck(USSR, 1)
    }
    true
  }
}

private object Card059FlowerPower extends Card(59, 4, USSR, true) {
  override def canEvent(game: Game, faction: Faction) = !game.flags.hasFlag(Flags.EvilEmpire)
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    game.addFlag(US, Flags.FlowerPower)
    true
  }
  def triggerFlowerPowerEffect(game: Game, faction: Faction): Unit = {  // TODO not finished To war cards
    if (game.stateStack.top == State.selectCardAndAction) { // Except space
      return
    }
    if (game.flags.hasFlag(faction, Flags.FlowerPower)) {
      game.addVpAndCheck(Faction.getOpposite(faction), 2)
    }
  }
}

private object Card060U2Incident extends Card(60, 3, USSR, true) {
  override def instantEvent(game: Game, faction: Faction): Boolean = { // TODO not finished To #32 UN
    game.addVpAndCheck(USSR, 1)
    game.addFlag(USSR, Flags.U2Incident)
    true
  }
}

private object Card061Opec extends Card(61, 3, USSR, false) {
  val countries = Set("Egypt", "Iran", "Libya", "Saudi Arabia", "Iraq", "Gulf States", "Venezuela")
  override def canEvent(game: Game, faction: Faction) = !game.flags.hasFlag(Flags.NorthSeaOil)
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    val vp = countries.count(game.worldMap.countries(_).getController == USSR)
    game.addVpAndCheck(USSR, vp)
    true
  }
}

private object Card064PanamaCanalReturned extends Card(64, 1, US, true) {
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    game.modifyInfluence(US, true, Map(
      game.worldMap.countries("Panama") -> 1,
      game.worldMap.countries("Venezuela") -> 1,
      game.worldMap.countries("Costa Rica") -> 1
    ))
    true
  }
}

private object Card065CampDavidAccords extends Card(65, 2, US, true) {
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    game.addVpAndCheck(US, 1)
    game.modifyInfluence(US, true, Map(
      game.worldMap.countries("Israel") -> 1,
      game.worldMap.countries("Jordan") -> 1,
      game.worldMap.countries("Egypt") -> 1
    ))
    game.addFlag(US, Flags.CampDavid)
    true
  }
}

private object Card068JohnPaulII extends Card(68, 2, US, true) {
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    val poland = game.worldMap.countries("Poland")
    game.modifyInfluence(USSR, false, Map(poland -> 2))
    game.modifyInfluence(US, true, Map(poland -> 1))
    game.addFlag(US, Flags.JohnPaulII)
    true
  }
}

private object Card071NixonPlaysTheChinaCard extends Card(71, 2, US, true) {
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    val chinaCard = Cards.chinaCard
    if (game.hand(USSR).has(chinaCard)) {
      game.hand(USSR).remove(chinaCard)
      game.discardCard(chinaCard, USSR, true, true)
    } else {
      game.addVpAndCheck(US, 2)
    }
    true
  }
}

private object Card072SadatExpelsSoviets extends Card(72, 1, US, true) {
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    val egypt = game.worldMap.countries("Egypt")
    game.modifyInfluence(USSR, false, Map(egypt -> egypt.influence(USSR)))
    game.modifyInfluence(US, true, Map(egypt -> 1))
    true
  }
}

private object Card073ShuttleDiplomacy extends Card(73, 3, US, true) { // TODO not finished
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    game.addFlag(US, Flags.ShuttleDiplomacy)
    true
  }
}

private object Card078AllianceForProgress extends Card(78, 3, US, true) {
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    val vp = game.worldMap.countries.values.count(country => {
      country.isBattlefield &&
        (country.regions.contains(Region.MidAmerica) || country.regions.contains(Region.SouthAmerica)) &&
        country.getController == US
    })
    game.addVpAndCheck(US, vp)
    true
  }
}

private object Card080SmallStep extends Card(80, 2, Neutral, false) {
  override def canEvent(game: Game, faction: Faction) =
    game.space(faction).level < game.space(Faction.getOpposite(faction)).level
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    game.increaseSpace(faction, 2)
    true
  }
}

private object Card082IranianHostage extends Card(82, 3, USSR, true) {
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    val iran = game.worldMap.countries("Iran")
    game.modifyInfluence(US, false, Map(iran -> iran.influence(US)))
    game.modifyInfluence(USSR, true, Map(iran -> 2))
    game.addFlag(US, Flags.IranianHostage)
    true
  }
}

private object Card083TheIronLady extends Card(83, 3, US, true) {
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    game.addVpAndCheck(US, 1)
    game.modifyInfluence(USSR, true, Map(game.worldMap.countries("Argentina") -> 1))
    val uk = game.worldMap.countries("UK")
    game.modifyInfluence(USSR, false, Map(uk -> uk.influence(USSR)))
    game.addFlag(US, Flags.IronLady)
    true
  }
}

private object Card084ReaganBombsLibya extends Card(84, 2, US, true) {
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    game.addVpAndCheck(US, game.worldMap.countries("Libya").influence(USSR) / 2)
    true
  }
}

private object Card086NorthSeaOil extends Card(86, 3, US, true) {
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    game.addFlag(US, Flags.NorthSeaOil)
    game.addFlag(US, Flags.NorthSeaOil8Rounds)
    true
  }
}

private object Card092Terrorism extends Card(92, 2, Neutral, false) {
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    val opposite = Faction.getOpposite(faction)
    val count = if (game.flags.hasFlag(opposite, Flags.IranianHostage)) 2 else 1
    val hand = game.hand(opposite)
    for (i <- 0 until count) {
      if (!hand.isEmptyExcludingChinaCard) {
        val card = hand.pickAndRemove(game.random)
        game.discardCard(card, opposite, true, true)
      }
    }
    true
  }
}

private object Card093IranContra extends Card(93, 2, USSR, true) {
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    game.addFlag(US, Flags.IranContra)
    true
  }
}

private object Card097EvilEmpire extends Card(97, 3, US, true) {
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    game.addVpAndCheck(US, 1)
    game.removeFlag(US, Flags.FlowerPower)
    game.addFlag(US, Flags.EvilEmpire)
    true
  }
}

private object Card100WarGames extends Card(100, 4, Neutral, true) {
  override def canEvent(game: Game, faction: Faction) = game.defcon == 2
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    game.addVpAndCheck(Faction.getOpposite(faction), 6)
    game.endGameByVp()
    true
  }
}

private object Card101Solidarity extends Card(101, 2, US, true) {
  override def canEvent(game: Game, faction: Faction) = game.flags.hasFlag(Flags.JohnPaulII)
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    val poland = game.worldMap.countries("Poland")
    game.modifyInfluence(US, true, Map(poland -> 3))
    true
  }
}

private object Card103Defectors extends Card(103, 2, US, false) {
  override def canHeadline(game: Game, faction: Faction) = true
  override def canEvent(game: Game, faction: Faction) = false
  override def afterPlay(game: Game, faction: Faction): Unit = {   // TODO Cancel headline effect
    if (faction == USSR) {
      if (game.stateStack.top == State.selectCardAndAction) { // Except space
        return
      }
      game.addVpAndCheck(US, 1)
    }
  }
}

private class DefaultCard(id: Int, op: Int, cardType: Faction) extends Card(id, op, cardType, false)

object Cards {
  private val cardMap = mutable.Map[Int, Card]()

  def addCard(id: Int, op: Int, cardType: Faction): Unit = cardMap += id -> new DefaultCard(id, op, cardType)
  def addCard(card: Card): Unit = cardMap += card.id -> card

  addCard(CardUnknown)
  addCard(new CardScoring(1, Region.Asia, 3, 7, 9))
  addCard(new CardScoring(2, Region.Europe, 3, 7, 1000))
  addCard(new CardScoring(3, Region.MidEast, 3, 5, 7))
  addCard(Card004DuckNCover)
  addCard(Card005FiveYearPlan)
  addCard(Card006ChinaCard)
  addCard(7, 3, USSR)
  addCard(Card008Fidel)
  addCard(Card009VietnamRevolts)
  addCard(10, 1, USSR)
  addCard(Card011KoreanWar)
  addCard(Card012RomanianAbdication)
  addCard(Card013ArabIsraeliWar)
  addCard(14, 3, USSR)
  addCard(Card015Nasser)
  addCard(16, 3, USSR)
  addCard(Card017DeGaulle)
  addCard(Card018CaptureNazi)
  addCard(19, 1, US)
  addCard(20, 2, Neutral)
  addCard(21, 4, US)
  addCard(22, 2, US)
  addCard(23, 4, US)
  addCard(24, 2, Neutral)
  addCard(Card025Containment)
  addCard(26, 1, US)
  addCard(Card027USJapanPact)
  addCard(28, 3, USSR)
  addCard(29, 3, US)
  addCard(30, 2, USSR)
  addCard(Card031RedScarePurge)
  addCard(32, 1, Neutral)
  addCard(33, 3, USSR)
  addCard(Card034NuclearTestBan)
  addCard(Card035Taiwan)
  addCard(36, 3, Neutral)
  addCard(new CardScoring(37, Region.MidAmerica, 1, 3, 5))
  addCard(Card038SEAsiaScoring)
  addCard(Card039ArmsRace)
  addCard(Card040CubaMissile)
  addCard(Card041NuclearSubs)
  addCard(Card042Quagmire)
  addCard(43, 3, Neutral)
  addCard(Card044BearTrap)
  addCard(45, 1, Neutral)
  addCard(46, 2, Neutral)
  addCard(47, 2, Neutral)
  addCard(Card048KitchenDebates)
  addCard(49, 2, Neutral)
  addCard(Card050WeWillBuryYou)
  addCard(Card051BrezhnevDoctrine)
  addCard(52, 2, USSR)
  addCard(53, 2, USSR)
  addCard(Card054Allende)
  addCard(Card055WillyBrandt)
  addCard(56, 4, USSR)
  addCard(57, 4, Neutral)
  addCard(Card058CulturalRevolution)
  addCard(Card059FlowerPower)
  addCard(Card060U2Incident)
  addCard(Card061Opec)
  addCard(62, 1, USSR)
  addCard(63, 2, US)
  addCard(Card064PanamaCanalReturned)
  addCard(Card065CampDavidAccords)
  addCard(66, 2, US)
  addCard(67, 2, US)
  addCard(Card068JohnPaulII)
  addCard(69, 2, Neutral)
  addCard(70, 1, US)
  addCard(Card071NixonPlaysTheChinaCard)
  addCard(Card072SadatExpelsSoviets)
  addCard(Card073ShuttleDiplomacy)
  addCard(74, 2, US)
  addCard(75, 2, USSR)
  addCard(76, 3, US)
  addCard(77, 3, US)
  addCard(Card078AllianceForProgress)
  addCard(new CardScoring(79, Region.Africa, 1, 4, 6))
  addCard(Card080SmallStep)
  addCard(new CardScoring(81, Region.SouthAmerica, 2, 5, 6))
  addCard(Card082IranianHostage)
  addCard(Card083TheIronLady)
  addCard(Card084ReaganBombsLibya)
  addCard(85, 2, US)
  addCard(Card086NorthSeaOil)
  addCard(87, 3, USSR)
  addCard(88, 2, USSR)
  addCard(89, 4, US)
  addCard(90, 4, USSR)
  addCard(91, 2, USSR)
  addCard(Card092Terrorism)
  addCard(Card093IranContra)
  addCard(94, 3, US)
  addCard(95, 2, USSR)
  addCard(96, 3, US)
  addCard(Card097EvilEmpire)
  addCard(98, 3, USSR)
  addCard(99, 3, USSR)
  addCard(Card100WarGames)
  addCard(Card101Solidarity)
  addCard(102, 2, Neutral)
  addCard(Card103Defectors)
  addCard(104, 2, USSR)
  addCard(105, 2, US)
  addCard(106, 3, US)
  addCard(107, 3, USSR)
  addCard(108, 2, US)
  addCard(109, 2, USSR)
  addCard(110, 3, US)

  println("Featured Cards: " + cardMap.values.count(!_.isInstanceOf[DefaultCard]))

  def fromId(id: Int): Card = cardMap(id)

  def earlyWarSet = cardMap.filter(e => (e._1 > 0 && e._1 <= 35 && e._1 != 6) || e._1 == 103).values
  def midWarSet = cardMap.filter(e => e._1 > 35 && e._1 <= 81).values
  def lateWarSet = cardMap.filter(e => e._1 > 81 && e._1 <= 102).values
  def chinaCard = cardMap(6)

  def isEarlyWarCard(card: Card): Boolean = {
    val i = card.id
    i <= 35 || (i >= 103 && i <= 106)
  }

  def isMidWarCard(card: Card): Boolean = {
    val i = card.id
    (i >= 36 && i <= 81) || (i >= 107 && i <= 108)
  }

  def isLateWarCard(card: Card): Boolean = {
    val i = card.id
    (i >= 82 && i <= 102) || (i >= 109 && i <= 110)
  }

  def getCardPeriod(card: Card): Int =
    if (isEarlyWarCard(card)) 1
    else if (isMidWarCard(card)) 2
    else if (isLateWarCard(card)) 3
    else 4
}
