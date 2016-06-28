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

class Flag (val flagType: FlagType, val isGoodFlag: Boolean, val priority: Int = 100) extends Ordered[Flag] {
  val id = Flag.newFlagId()
  def canRealignmentOrCoup(country: Country): Option[Boolean] = None
  def canRealignment(country: Country) = canRealignmentOrCoup(country)
  def canCoup(country: Country) = canRealignmentOrCoup(country)
  override def compare(that: Flag): Int =
    if (priority > that.priority) -1 else if (priority < that.priority) 1 else
      if (id > that.id) 1 else if (id < that.id) -1 else 0
  override def toString = f"Flag($id)"
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
  val flagSets = Map[Faction, mutable.Set[Flag]](
    US -> mutable.Set(),
    USSR -> mutable.Set(),
    Neutral -> mutable.Set()
  )
  val flagSets2 = Map[Faction, mutable.Set[Flag]](
    US -> mutable.SortedSet(),
    USSR -> mutable.SortedSet()
  )
  def addFlag(faction: Faction, flag: Flag): Unit = {
    flagSets(faction) += flag
    if (faction == Neutral) {
      flagSets2.foreach(_._2 += flag)
    } else {
      flagSets2(faction) += flag
    }
  }
  def removeFlag(faction: Faction, flag: Flag): Unit = {
    flagSets(faction) -= flag
    if (faction == Neutral) {
      flagSets2.foreach(_._2 -= flag)
    } else {
      flagSets2(faction) -= flag
    }
  }
  def hasFlag(faction: Faction, flag: Flag): Boolean = {
    flagSets2(faction)(flag)
  }
  def hasFlag(flag: Flag): Boolean = {
    flagSets.exists(_._2(flag))
  }
  def turnEnds(): Unit = {
    flagSets.foreach(_._2.retain(_.flagType == Always))
    flagSets2.foreach(_._2.retain(_.flagType == Always))
  }
}

object Flags {
  val Space1 = Flag(ThisTurn, false)
  val Space2 = Flag(ThisTurn, false)
  val SpaceAwardTwoSpace = Flag(Always, true)
  val SpaceAwardHeadlineThen = Flag(Always, true)
  val SpaceAwardMayDiscard = Flag(Always, true)
  val SpaceAwardTake8Rounds = Flag(Always, true)
  val CantPlayChinaCard = Flag(ThisTurn, false)
  val Defcon4Penalty = new Flag(Always, false) {
    override def canRealignmentOrCoup(country: Country) = if (country.regions(Region.Europe)) Some(false) else None
  }
  val Defcon3Penalty = new Flag(Always, false) {
    override def canRealignmentOrCoup(country: Country) = if (country.regions(Region.Asia)) Some(false) else None
  }
  val Defcon2Penalty = new Flag(Always, false) {
    override def canRealignmentOrCoup(country: Country) = if (country.regions(Region.MidEast)) Some(false) else None
  }
  val VietnamRevolts = Flag(ThisTurn, true)
  val DeGaulle = new Flag(Always, true, 80) {
    override def canRealignmentOrCoup(country: Country) = if (country.name == "France") Some(true) else None
  }
  val Containment = Flag(ThisTurn, true)
  val USJapanPact = new Flag(Always, false, 60) {
    override def canRealignmentOrCoup(country: Country) = if (country.name == "Japan") Some(false) else None
  }
  val RedScarePurge = Flag(ThisTurn, false)
  val Taiwan = Flag(Always, true)
  val CubaMissile = Flag(ThisTurn, false)
  val NuclearSubs = Flag(ThisTurn, true)
  val QuagmireBearTrap = Flag(Always, false)
  val WeWillBuryYou = Flag(Always, false)
  val WillyBrandt = new Flag(Always, true, 80) {
    override def canRealignmentOrCoup(country: Country) = if (country.name == "W.Germany") Some(true) else None
  }
  val FlowerPower = Flag(Always, false)
  val U2Incident = Flag(ThisTurn, true)
  val CampDavid = Flag(Always, true)
  val JohnPaulII = Flag(Always, true)
  val ShuttleDiplomacy = Flag(Always, true)
  val IranianHostage = Flag(Always, false)
  val IronLady = Flag(Always, true)
  val NorthSeaOil = Flag(Always, true)
  val NorthSeaOil8Rounds = Flag(ThisTurn, true)
  val IranContra = Flag(ThisTurn, false)
  val EvilEmpire = Flag(Always, true)
  val WarsawPact = Flag(Always, true)
  val NATO = new Flag(Always, false, 60) {
    override def canRealignmentOrCoup(country: Country) =
      if (country.regions(Region.Europe) && country.getController == US) Some(false) else None
  }
  val MarshallPlan = Flag(Always, true)

  def init(): Unit = {}
}
