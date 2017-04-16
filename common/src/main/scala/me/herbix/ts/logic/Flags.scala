package me.herbix.ts.logic

import me.herbix.ts.logic.FlagType.FlagType

import me.herbix.ts.logic.Faction._
import me.herbix.ts.logic.card.Card049MissileEnvy
import me.herbix.ts.logic.chinesecivilwar.CCWFlags

import scala.collection.mutable

/**
  * Created by Chaofan on 2016/6/12.
  */
object FlagType extends Enumeration {
  type FlagType = Value
  val Always, ThisTurn, DuringSetup = Value
}

import FlagType._

class Flag (val flagType: FlagType, val isGoodFlag: Boolean, val priority: Int = 100) extends Ordered[Flag] {
  val id = FlagsTrait.newFlagId()
  FlagsTrait.flags(id) = this

  def canRealignmentOrCoup(game: Game, country: Country): Option[Boolean] = canRealignmentOrCoup(country)
  def canRealignment(game: Game, country: Country): Option[Boolean] = canRealignment(country)
  def canCoup(game: Game, country: Country): Option[Boolean] = canCoup(country)

  protected def canRealignmentOrCoup(country: Country): Option[Boolean] = None
  protected def canRealignment(country: Country): Option[Boolean] = canRealignmentOrCoup(country)
  protected def canCoup(country: Country): Option[Boolean] = canRealignmentOrCoup(country)
  def canKeep(game: Game, faction: Faction) = true
  override def compare(that: Flag): Int =
    if (priority > that.priority) -1 else if (priority < that.priority) 1 else
      if (id > that.id) 1 else if (id < that.id) -1 else 0
  override def toString = f"Flag($id)"
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
  val flagData = Map[Faction, mutable.Map[Flag, Any]](
    US -> mutable.Map(),
    USSR -> mutable.Map(),
    Neutral -> mutable.Map()
  )
  def addFlag(faction: Faction, flag: Flag, data: Any = null): Unit = {
    flagSets(faction) += flag
    flagData(faction)(flag) = data
    if (faction == Neutral) {
      flagSets2.foreach(_._2 += flag)
    } else {
      flagSets2(faction) += flag
    }
  }
  def removeFlag(faction: Faction, flag: Flag): Unit = {
    flagSets(faction) -= flag
    flagData(faction) -= flag
    if (faction == Neutral) {
      flagSets2.foreach(_._2 -= flag)
    } else {
      flagSets2(faction) -= flag
    }
  }
  def hasFlag(faction: Faction, flag: Flag): Boolean = {
    if (faction != Neutral) {
      flagSets2(faction)(flag)
    } else {
      flagSets(Neutral)(flag)
    }
  }
  def hasFlag(flag: Flag): Boolean = {
    flagSets.exists(_._2(flag))
  }
  def setFlagData(faction: Faction, flag: Flag, data: Any): Unit = {
    if (hasFlag(faction, flag)) flagData(faction)(flag) = data
  }
  def getFlagData(faction: Faction, flag: Flag): Any = {
    if (hasFlag(faction, flag)) flagData(faction)(flag) else null
  }
  def turnEnds(): Unit = {
    flagSets.foreach(_._2.retain(_.flagType == Always))
    flagSets2.foreach(_._2.retain(_.flagType == Always))
  }
}

object FlagsTrait {
  var flagId = -1
  def newFlagId() = {
    flagId += 1
    flagId
  }

  val flags = mutable.Map[Int, Flag]()
  def fromId(id: Int) = {
    flags.getOrElse(id, null)
  }
}

trait FlagsTrait {
  def init(): Unit = {}
}

object Flags extends FlagsTrait {
  val Space1 = new Flag(ThisTurn, false)
  val Space2 = new Flag(ThisTurn, false)
  val SpaceAwardTwoSpace = new Flag(Always, true)
  val SpaceAwardHeadlineThen = new Flag(Always, true)
  val SpaceAwardMayDiscard = new Flag(Always, true)
  val SpaceAwardTake8Rounds = new Flag(Always, true)
  val CantPlayChinaCard = new Flag(ThisTurn, false)
  val Defcon4Penalty = new Flag(Always, false) {
    override def canRealignmentOrCoup(country: Country) = if (country.regions(Region.Europe)) Some(false) else None
  }
  val Defcon3Penalty = new Flag(Always, false) {
    override def canRealignmentOrCoup(country: Country) = if (country.regions(Region.Asia)) Some(false) else None
  }
  val Defcon2Penalty = new Flag(Always, false) {
    override def canRealignmentOrCoup(country: Country) = if (country.regions(Region.MidEast)) Some(false) else None
  }
  val VietnamRevolts = new Flag(ThisTurn, true)
  val DeGaulle = new Flag(Always, true, 80) {
    override def canRealignmentOrCoup(country: Country) = if (country.name == "France") Some(true) else None
  }
  val Containment = new Flag(ThisTurn, true)
  val USJapanPact = new Flag(Always, false, 60) {
    override def canRealignmentOrCoup(country: Country) = if (country.name == "Japan") Some(false) else None
  }
  val RedScarePurge = new Flag(ThisTurn, false)
  val Taiwan = new Flag(Always, true)
  val CubaMissile = new Flag(ThisTurn, false)
  val NuclearSubs = new Flag(ThisTurn, true)
  val QuagmireBearTrap = new Flag(Always, false)
  val WeWillBuryYou = new Flag(Always, false)
  val WillyBrandt = new Flag(Always, true, 80) {
    override def canRealignmentOrCoup(country: Country) = if (country.name == "W.Germany") Some(true) else None
  }
  val FlowerPower = new Flag(Always, false)
  val U2Incident = new Flag(ThisTurn, true)
  val CampDavid = new Flag(Always, true)
  val JohnPaulII = new Flag(Always, true)
  val ShuttleDiplomacy = new Flag(Always, true)
  val IranianHostage = new Flag(Always, false)
  val IronLady = new Flag(Always, true)
  val NorthSeaOil = new Flag(Always, true)
  val NorthSeaOil8Rounds = new Flag(ThisTurn, true)
  val IranContra = new Flag(ThisTurn, false)
  val EvilEmpire = new Flag(Always, true)
  val WarsawPact = new Flag(Always, true)
  val NATO = new Flag(Always, false, 60) {
    override def canRealignmentOrCoup(game: Game, country: Country) =
      if (country.regions(Region.Europe) && game.getController(country) == US) Some(false) else None
  }
  val MarshallPlan = new Flag(Always, true)
  val SALT = new Flag(ThisTurn, false)
  val MissileEnvy = new Flag(Always, false) {
    override def canKeep(game: Game, faction: Faction) = game.hand(faction).has(Card049MissileEnvy)
  }
  val DeathSquads = new Flag(ThisTurn, true)
  val DeathSquads2 = new Flag(ThisTurn, false)
  val Reformer = new Flag(Always, true, 90) {
    override def canCoup(country: Country) = if (country.regions(Region.Europe)) Some(false) else None
  }
  val Chernobyl = new Flag(ThisTurn, false)
  val TearDownThisWall = new Flag(Always, true)
  val AldrichAmes = new Flag(ThisTurn, false)
  val NORAD = new Flag(Always, true)
  val Samantha = new Flag(ThisTurn, true)
  val AwacsSale = new Flag(Always, true)
}
