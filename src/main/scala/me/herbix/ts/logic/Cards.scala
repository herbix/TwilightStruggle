package me.herbix.ts.logic

import me.herbix.ts.logic.Faction._
import me.herbix.ts.logic.Region.Region

import scala.collection.mutable

/**
  * Created by Chaofan on 2016/6/17.
  */
abstract class Card(val id: Int, val op: Int, val faction: Faction, val isRemovedAfterEvent: Boolean) {
  def canHeadline(game: Game, faction: Faction) = canEvent(game, faction)
  def canDiscard(game: Game) = true
  def canEvent(game: Game, faction: Faction) = true
  def canPlay(game: Game, faction: Faction) = true
  def canHeld(game: Game) = true
  def instantEvent(game: Game): Boolean = true
  def modifyOp(faction: Faction, originalOp: Int, targets: Iterable[Country]): Int = originalOp
  def modifyOp(faction: Faction, originalOp: Int): Int = originalOp
  def nextState(game: Game, input: Operation): Unit = {
    if (game.stateStack.top == State.cardEventStart) {
      if (instantEvent(game)) {
        game.stateStack.pop()
        game.stateStack.push(State.cardEventEnd)
      }
    }
  }
}

object CardUnknown extends Card(0, 0, Neutral, false)

class CardScoring(id: Int, val region: Region, val presence: Int, val domination: Int, val control: Int)
  extends Card(id, 0, Neutral, false) {
  override def canHeld(game: Game) = true
  override def instantEvent(game: Game): Boolean = {
    game.scoring(region, presence, domination, control)
    game.checkVp()
    true
  }
}

object Card004DuckNCover extends Card(4, 3, US, false) {
  override def instantEvent(game: Game): Boolean = {
    game.setDefcon(game.defcon - 1)
    game.addVp(US, 5 - game.defcon)
    true
  }
}

object Card005FiveYearPlan extends Card(5, 3, US, false) {
  override def instantEvent(game: Game): Boolean = {
    val ussrHand = game.hand(USSR)
    if (ussrHand.isEmptyExcludingChinaCard) {
      return true
    }
    val card = ussrHand.pickAndRemove(game.random)
    val activateEvent = card.faction == US
    game.discardCard(card, USSR, !activateEvent, true)
    if (activateEvent) {
      game.currentCard = card
      card.nextState(game, null)
      false
    } else {
      true
    }
  }
}

object Card006ChinaCard extends Card(6, 4, Neutral, false) {
  override def canEvent(game: Game, faction: Faction) = false
  override def canPlay(game: Game, faction: Faction) = !game.flags.hasFlag(faction, Flags.cantPlayChinaCard)
  override def canDiscard(game: Game) = false
  override def modifyOp(faction: Faction, originalOp: Int, targets: Iterable[Country]): Int =
    if (targets.forall(_.regions.contains(Region.Asia))) {
      originalOp + 1
    } else {
      originalOp
    }
}

object Card008Fidel extends Card(8, 2, USSR, true) {
  override def instantEvent(game: Game): Boolean = {
    val cuba = game.worldMap.countries("Cuba")
    game.modifyInfluence(US, false, Map(cuba -> cuba.influence(US)))
    game.modifyInfluence(USSR, true, Map(cuba -> cuba.stability))
    true
  }
}

object Card009VietnamRevolts extends Card(9, 2, USSR, true) {
  override def instantEvent(game: Game): Boolean = {
    val vietnam = game.worldMap.countries("Vietnam")
    game.modifyInfluence(USSR, true, Map(vietnam -> 2))
    game.flags.addFlag(USSR, Flags.VietnamRevolts)
    true
  }
}

object Card011KoreanWar extends Card(11, 2, USSR, true) {
  override def instantEvent(game: Game): Boolean = {
    val southKorea = game.worldMap.countries("S.Korea")
    val modifier = game.worldMap.links(southKorea.name).count(game.worldMap.countries(_).getController == US)
    game.war(USSR, southKorea, modifier, 4, 2, 2)
    true
  }
}

object Card012RomanianAbdication extends Card(12, 1, USSR, true) {
  override def instantEvent(game: Game): Boolean = {
    val romania = game.worldMap.countries("Romania")
    game.modifyInfluence(US, false, Map(romania -> romania.influence(US)))
    game.modifyInfluence(USSR, true, Map(romania -> romania.stability))
    true
  }
}

object Card013ArabIsraeliWar extends Card(13, 2, USSR, false) {
  override def instantEvent(game: Game): Boolean = {
    val israel = game.worldMap.countries("Israel")
    val modifier = game.worldMap.links(israel.name).count(game.worldMap.countries(_).getController == US) +
      (if (israel.getController == US) 1 else 0)
    game.war(USSR, israel, modifier, 4, 2, 2)
    true
  }
}

object Card015Nasser extends Card(15, 1, USSR, true) {
  override def instantEvent(game: Game): Boolean = {
    val egypt = game.worldMap.countries("Egypt")
    game.modifyInfluence(US, false, Map(egypt -> (egypt.influence(US) + 1) / 2))
    game.modifyInfluence(USSR, true, Map(egypt -> 2))
    true
  }
}

object Card017DeGaulle extends Card(17, 3, USSR, true) {
  override def instantEvent(game: Game): Boolean = {
    val france = game.worldMap.countries("France")
    game.modifyInfluence(US, false, Map(france -> 2))
    game.modifyInfluence(USSR, true, Map(france -> 1))
    game.flags.addFlag(USSR, Flags.DeGaulle)
    true
  }
}

object Card018CaptureNazi extends Card(18, 1, Neutral, true) {
  override def instantEvent(game: Game): Boolean = {
    game.increaseSpace(game.phasingPlayer, 1)
    true
  }
}

object Card025Containment extends Card(25, 3, US, true) {
  override def instantEvent(game: Game): Boolean = {
    game.flags.addFlag(US, Flags.Containment)
    true
  }
}

object Card027USJapanPact extends Card(27, 4, US, true) {
  override def instantEvent(game: Game): Boolean = {
    val japan = game.worldMap.countries("Japan")
    val change = japan.influence(USSR) + japan.stability - japan.influence(US)
    if (change > 0) {
      game.modifyInfluence(USSR, true, Map(japan -> change))
    }
    game.flags.addFlag(USSR, Flags.USJapanPact)
    true
  }
}

class DefaultCard(id: Int, op: Int, cardType: Faction) extends Card(id, op, cardType, false)

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
  addCard(31, 4, Neutral)
  addCard(32, 1, Neutral)
  addCard(33, 3, USSR)
  addCard(34, 4, Neutral)
  addCard(35, 2, US)
  addCard(36, 3, Neutral)
  addCard(37, 0, Neutral)
  addCard(38, 0, Neutral)
  addCard(39, 3, Neutral)
  addCard(40, 3, Neutral)
  addCard(41, 2, US)
  addCard(42, 3, USSR)
  addCard(43, 3, Neutral)
  addCard(44, 3, US)
  addCard(45, 1, Neutral)
  addCard(46, 2, Neutral)
  addCard(47, 2, Neutral)
  addCard(48, 1, US)
  addCard(49, 2, Neutral)
  addCard(50, 4, USSR)
  addCard(51, 3, USSR)
  addCard(52, 2, USSR)
  addCard(53, 2, USSR)
  addCard(54, 1, USSR)
  addCard(55, 2, USSR)
  addCard(56, 4, USSR)
  addCard(57, 4, Neutral)
  addCard(58, 3, USSR)
  addCard(59, 4, USSR)
  addCard(60, 3, USSR)
  addCard(61, 3, USSR)
  addCard(62, 1, USSR)
  addCard(63, 2, US)
  addCard(64, 1, US)
  addCard(65, 2, US)
  addCard(66, 2, US)
  addCard(67, 2, US)
  addCard(68, 2, US)
  addCard(69, 2, Neutral)
  addCard(70, 1, US)
  addCard(71, 2, US)
  addCard(72, 1, US)
  addCard(73, 3, US)
  addCard(74, 2, US)
  addCard(75, 2, USSR)
  addCard(76, 3, US)
  addCard(77, 3, US)
  addCard(78, 3, US)
  addCard(79, 0, Neutral)
  addCard(80, 2, Neutral)
  addCard(81, 0, Neutral)
  addCard(82, 3, USSR)
  addCard(83, 3, US)
  addCard(84, 2, US)
  addCard(85, 2, US)
  addCard(86, 3, US)
  addCard(87, 3, USSR)
  addCard(88, 2, USSR)
  addCard(89, 4, US)
  addCard(90, 4, USSR)
  addCard(91, 2, USSR)
  addCard(92, 2, Neutral)
  addCard(93, 2, USSR)
  addCard(94, 3, US)
  addCard(95, 2, USSR)
  addCard(96, 3, US)
  addCard(97, 3, US)
  addCard(98, 3, USSR)
  addCard(99, 3, USSR)
  addCard(100, 4, Neutral)
  addCard(101, 2, US)
  addCard(102, 2, Neutral)
  addCard(103, 2, US)
  addCard(104, 2, USSR)
  addCard(105, 2, US)
  addCard(106, 3, US)
  addCard(107, 3, USSR)
  addCard(108, 2, US)
  addCard(109, 2, USSR)
  addCard(110, 3, US)

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
