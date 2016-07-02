package me.herbix.ts.logic

import me.herbix.ts.logic.Faction._

/**
  * Created by Chaofan on 2016/6/13.
  */
object Region extends Enumeration {
  type Region = Value
  val Europe, MidEast, Asia, Africa, MidAmerica, SouthAmerica = Value
  val EastEurope, WestEurope, SouthEastAsia = Value
  val Super = Value

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

  import RegionState._

  def getRegionState(game: Game, region: Region): Map[Faction, RegionState] = {
    val targetCountries = game.worldMap.countries.values.filter(_.regions(region))
    val battlefieldCount = targetCountries.count(_.isBattlefield)
    val usBattlefield = targetCountries.count(country => country.isBattlefield && country.getController == US)
    val usNonBattlefield = targetCountries.count(country => !country.isBattlefield && country.getController == US)
    val ussrBattlefield = targetCountries.count(country => country.isBattlefield && country.getController == USSR)
    val ussrNonBattlefield = targetCountries.count(country => !country.isBattlefield && country.getController == USSR)
    val usAll = usBattlefield + usNonBattlefield
    val ussrAll = ussrBattlefield + ussrNonBattlefield

    val usPresence = usBattlefield > 0 || usNonBattlefield > 0
    val ussrPresence = ussrBattlefield > 0 || ussrNonBattlefield > 0
    val usDomination = usBattlefield > ussrBattlefield && usAll > ussrAll && usNonBattlefield > 0
    val ussrDomination = ussrBattlefield > usBattlefield && ussrAll > usAll && ussrNonBattlefield > 0
    val usControl = usBattlefield == battlefieldCount && usAll > ussrAll
    val ussrControl = ussrBattlefield == battlefieldCount && ussrAll > usAll

    val usState = if (usControl) Control else if (usDomination) Domination else if (usPresence) Presence else Nop
    val ussrState = if (ussrControl) Control else if (ussrDomination) Domination else if (ussrPresence) Presence else Nop

    Map(US -> usState, USSR -> ussrState)
  }
}
