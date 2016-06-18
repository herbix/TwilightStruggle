package me.herbix.ts.logic

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
}
