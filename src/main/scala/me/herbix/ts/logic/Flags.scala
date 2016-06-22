package me.herbix.ts.logic

import me.herbix.ts.logic.FlagType.FlagType

import Faction._

import scala.collection.mutable

/**
  * Created by Chaofan on 2016/6/12.
  */
object FlagType extends Enumeration {
  type FlagType = Value
  val Always, ThisTurn = Value
}

import FlagType._

class Flag private(val flagType: FlagType, val isGoodFlag: Boolean) {
  val id = Flag.newFlagId()
}

object Flag {
  var flagId = -1
  def newFlagId() = {
    flagId += 1
    flagId
  }
  def apply(flagType: FlagType, isGoodFlag: Boolean) = new Flag(flagType, isGoodFlag)
}

class Flags {
  val flagSets = mutable.Map[Faction, mutable.Set[Flag]](
    US -> mutable.Set(),
    USSR -> mutable.Set(),
    Neutral -> mutable.Set()
  )
  def addFlag(faction: Faction, flag: Flag): Unit = {
    flagSets(faction) += flag
  }
  def removeFlag(faction: Faction, flag: Flag): Unit = {
    flagSets(faction) -= flag
  }
  def hasFlag(faction: Faction, flag: Flag): Boolean = {
    flagSets(faction).contains(flag) || flagSets(Neutral).contains(flag)
  }
  def turnEnds(): Unit = {
    flagSets.foreach(_._2.retain(_.flagType == Always))
  }
}

object Flags {
  val Space1 = Flag(ThisTurn, false)
  val Space2 = Flag(ThisTurn, false)
  val SpaceAwardTwoSpace = Flag(Always, true)
  val SpaceAwardHeadlineThen = Flag(Always, true)
  val SpaceAwardMayDiscard = Flag(Always, true)
  val SpaceAwardTwoTake8Rounds = Flag(Always, true)
  val cantPlayChinaCard = Flag(ThisTurn, false)
}
