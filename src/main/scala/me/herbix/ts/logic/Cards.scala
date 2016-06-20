package me.herbix.ts.logic

import me.herbix.ts.logic.Faction._

import scala.collection.mutable

/**
  * Created by Chaofan on 2016/6/17.
  */
abstract class Card(val id: Int, val op: Int, val faction: Faction)

class DefaultCard(id: Int, op: Int, cardType: Faction) extends Card(id, op, cardType)

object Cards {
  private var cardCount = 0
  private val cardMap = mutable.Map[Int, Card]()

  def addCard(op: Int, cardType: Faction): Unit = {
    cardMap += cardCount -> new DefaultCard(cardCount, op, cardType)
    cardCount += 1
  }

  addCard(0, Neutral)
  addCard(0, Neutral)
  addCard(0, Neutral)
  addCard(0, Neutral)
  addCard(3, US)
  addCard(3, US)
  addCard(4, Neutral)
  addCard(3, USSR)
  addCard(2, USSR)
  addCard(2, USSR)
  addCard(1, USSR)
  addCard(2, USSR)
  addCard(1, USSR)
  addCard(2, USSR)
  addCard(3, USSR)
  addCard(1, USSR)
  addCard(3, USSR)
  addCard(3, USSR)
  addCard(1, Neutral)
  addCard(1, US)
  addCard(2, Neutral)
  addCard(4, US)
  addCard(2, US)
  addCard(4, US)
  addCard(2, Neutral)
  addCard(3, US)
  addCard(1, US)
  addCard(4, US)
  addCard(3, USSR)
  addCard(3, US)
  addCard(2, USSR)
  addCard(4, Neutral)
  addCard(1, Neutral)
  addCard(3, USSR)
  addCard(4, Neutral)
  addCard(2, US)
  addCard(3, Neutral)
  addCard(0, Neutral)
  addCard(0, Neutral)
  addCard(3, Neutral)
  addCard(3, Neutral)
  addCard(2, US)
  addCard(3, USSR)
  addCard(3, Neutral)
  addCard(3, US)
  addCard(1, Neutral)
  addCard(2, Neutral)
  addCard(2, Neutral)
  addCard(1, US)
  addCard(2, Neutral)
  addCard(4, USSR)
  addCard(3, USSR)
  addCard(2, USSR)
  addCard(2, USSR)
  addCard(1, USSR)
  addCard(2, USSR)
  addCard(4, USSR)
  addCard(4, Neutral)
  addCard(3, USSR)
  addCard(4, USSR)
  addCard(3, USSR)
  addCard(3, USSR)
  addCard(1, USSR)
  addCard(2, US)
  addCard(1, US)
  addCard(2, US)
  addCard(2, US)
  addCard(2, US)
  addCard(2, US)
  addCard(2, Neutral)
  addCard(1, US)
  addCard(2, US)
  addCard(1, US)
  addCard(3, US)
  addCard(2, US)
  addCard(2, USSR)
  addCard(3, US)
  addCard(3, US)
  addCard(3, US)
  addCard(0, Neutral)
  addCard(2, Neutral)
  addCard(0, Neutral)
  addCard(3, USSR)
  addCard(3, US)
  addCard(2, US)
  addCard(2, US)
  addCard(3, US)
  addCard(3, USSR)
  addCard(2, USSR)
  addCard(4, US)
  addCard(4, USSR)
  addCard(2, USSR)
  addCard(2, Neutral)
  addCard(2, USSR)
  addCard(3, US)
  addCard(2, USSR)
  addCard(3, US)
  addCard(3, US)
  addCard(3, USSR)
  addCard(3, USSR)
  addCard(4, Neutral)
  addCard(2, US)
  addCard(2, Neutral)
  addCard(2, US)
  addCard(2, USSR)
  addCard(2, US)
  addCard(3, US)
  addCard(3, USSR)
  addCard(2, US)
  addCard(2, USSR)
  addCard(3, US)

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
