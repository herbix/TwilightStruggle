package me.herbix.ts.logic

/**
  * Created by Chaofan on 2016/6/12.
  */
object Faction extends Enumeration {
  type Faction = Value
  val US, USSR, Neutral = Value

  def getOpposite(faction: Faction): Faction = faction match {
    case US => USSR
    case USSR => US
    case _ => Neutral
  }
}
