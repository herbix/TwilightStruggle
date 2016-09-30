package me.herbix.ts.logic

import me.herbix.ts.logic.Faction._

/**
  * Created by Chaofan on 2016/6/13.
  */
object Region extends Enumeration {
  type Region = Value
  val Europe, MidEast, Asia, Africa, MidAmerica, SouthAmerica = Value
  val EastEurope, WestEurope, SouthEastAsia = Value
  val Special = Value

  val WestEuropeSet = Set(WestEurope, Europe)
  val EastEuropeSet = Set(EastEurope, Europe)
  val MidEuropeSet = Set(WestEurope, EastEurope, Europe)
  val SouthEastAsiaSet = Set(SouthEastAsia, Asia)

  val MainRegionSet = Set(Europe, MidEast, Asia, Africa, MidAmerica, SouthAmerica)
  val ScoringInfo = Map(
    Asia -> (3, 7, 9),
    Europe -> (3, 7, 1000),
    MidEast -> (3, 5, 7),
    MidAmerica -> (1, 3, 5),
    Africa -> (1, 4, 6),
    SouthAmerica -> (2, 5, 6)
  )

  object RegionState extends Enumeration {
    type RegionState = Value
    val Nop, Presence, Domination, Control = Value
  }
}
