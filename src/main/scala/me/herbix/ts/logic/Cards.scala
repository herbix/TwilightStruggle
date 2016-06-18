package me.herbix.ts.logic

import me.herbix.ts.logic.Faction._

/**
  * Created by Chaofan on 2016/6/17.
  */
abstract class Card(val id: Int, val op: Int, val cardType: Faction)

class DefaultCard(id: Int, op: Int, cardType: Faction) extends Card(id, op, cardType)

object Cards {
  private val cardMap = (1 to 110).map(i => (i, new DefaultCard(i, 3, Faction.US))).toMap

  def fromId(id: Int): Card = cardMap(id)

  def earlyWarSet = cardMap.filter(e => (e._1 <= 35 && e._1 != 6) || e._1 == 103).values
  def midWarSet = cardMap.filter(e => e._1 > 35 && e._1 <= 81).values
  def lateWarSet = cardMap.filter(e => e._1 > 81 && e._1 <= 102).values
  def chinaCard = cardMap(6)
}
