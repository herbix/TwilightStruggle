package me.herbix.ts.logic.turnzero

import me.herbix.ts.logic.FlagType._
import me.herbix.ts.logic.{Country, Faction, Flag, FlagsTrait}

/**
  * Created by Chaofan on 2017/4/9.
  */
object TZFlags extends FlagsTrait {

  val FlagIdOffset = FlagsTrait.flagId

  // Yalta & Potsdam
  val ussrEuropePlus1 = new Flag(DuringSetup, true)
  val ussrVietnamOrArab = new Flag(DuringSetup, true)
  val usMarshall = new Flag(DuringSetup, true)
  val usGoesFirst = new Flag(Always, true)

  // VE day
  val ussrEastEuropePlus2 = new Flag(DuringSetup, true)
  val blockadeNegated = new Flag(Always, false)

  // 1945 UK Election
  val socialistGovernmentsNegated = new Flag(Always, false)
  val suezCrisisNegated = new Flag(Always, false)

  // Israel
  val ussrMidEastPlus2 = new Flag(DuringSetup, true)

  // Chinese civil war
  val battlefieldTaiwan = new Flag(Always, true)

  // VJ day
  val defconLock = new Flag(ThisTurn, true, 120) {
    override def canRealignmentOrCoup(country: Country) = Some(true)
  }

  val flagsSolveList = List(
    ussrVietnamOrArab,
    usMarshall,
    ussrEuropePlus1,
    ussrEastEuropePlus2,
    ussrMidEastPlus2
  )

  val flagsSolveFaction = Map(
    ussrEuropePlus1 -> Faction.USSR,
    ussrEastEuropePlus2 -> Faction.USSR,
    ussrMidEastPlus2 -> Faction.USSR,
    ussrVietnamOrArab -> Faction.USSR,
    usMarshall -> Faction.US

  )

}
