// Copyright (C) 2017 Chaofan

package me.herbix.ts.logic

/**
  * Created by Chaofan on 2016/6/12.
  */
object SpaceLevel {
  //                            |id|op|di|vp|vp|flag|
  val Start =     new SpaceLevel(0, 0, 0, 0, 0, null)
  val Satellite = new SpaceLevel(1, 2, 3, 2, 1, null)
  val Animal =    new SpaceLevel(2, 2, 4, 0, 0, Flags.SpaceAwardTwoSpace)
  val Man =       new SpaceLevel(3, 2, 3, 2, 0, null)
  val Orbit =     new SpaceLevel(4, 2, 4, 0, 0, Flags.SpaceAwardHeadlineThen)
  val Lunar =     new SpaceLevel(5, 3, 3, 3, 1, null)
  val Landed =    new SpaceLevel(6, 3, 4, 0, 0, Flags.SpaceAwardMayDiscard)
  val Shuttle =   new SpaceLevel(7, 3, 3, 4, 2, null)
  val Station =   new SpaceLevel(8, 4, 2, 2, 0, Flags.SpaceAwardTake8Rounds)

  val SpaceList = Array(
    Start, Satellite, Animal, Man, Orbit, Lunar, Landed, Shuttle, Station, null
  )

  def apply(level: Int) = SpaceList(level)

  class SpaceLevel(val level: Int, val op: Int, val rollMax: Int, val firstVp: Int, val secondVp: Int, val flag: Flag)
    extends Ordered[SpaceLevel] {
    override def compare(that: SpaceLevel): Int =
      if (level > that.level) 1
      else if (level < that.level) -1
      else 0
    def nextLevel = SpaceLevel(level + 1)
  }
}
