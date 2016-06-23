package me.herbix.ts.logic

import me.herbix.ts.logic.Faction._

import scala.collection.mutable

/**
  * Created by Chaofan on 2016/6/17.
  */
abstract class Card(val id: Int, val op: Int, val faction: Faction) {
  def canHeadline(game: Game): Boolean = true
  def canBeDiscarded(game: Game): Boolean = true
  def canPlayAsEvent(game: Game): Boolean = true
  def isRemovedAfterEvent = false
  def nextState(game: Game, input: Operation): Unit = {
    if (game.stateStack.top == State.cardEventStart) {
      game.stateStack.pop()
      game.stateStack.push(State.cardEventEnd)
    }
  }
}

class DefaultCard(id: Int, op: Int, cardType: Faction) extends Card(id, op, cardType)

object Cards {
  private val cardMap = mutable.Map[Int, Card]()

  def addCard(id: Int, op: Int, cardType: Faction): Unit = cardMap += id -> new DefaultCard(id, op, cardType)
  def addCard(card: Card): Unit = cardMap += card.id -> card

  addCard(0, 0, Neutral)
  addCard(1, 0, Neutral)
  addCard(2, 0, Neutral)
  addCard(3, 0, Neutral)
  addCard(4, 3, US)
  addCard(5, 3, US)
  addCard(6, 4, Neutral)
  addCard(7, 3, USSR)
  addCard(8, 2, USSR)
  addCard(9, 2, USSR)
  addCard(10, 1, USSR)
  addCard(11, 2, USSR)
  addCard(12, 1, USSR)
  addCard(13, 2, USSR)
  addCard(14, 3, USSR)
  addCard(15, 1, USSR)
  addCard(16, 3, USSR)
  addCard(17, 3, USSR)
  addCard(18, 1, Neutral)
  addCard(19, 1, US)
  addCard(20, 2, Neutral)
  addCard(21, 4, US)
  addCard(22, 2, US)
  addCard(23, 4, US)
  addCard(24, 2, Neutral)
  addCard(25, 3, US)
  addCard(26, 1, US)
  addCard(27, 4, US)
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
