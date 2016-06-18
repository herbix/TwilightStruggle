package me.herbix.ts.logic

import me.herbix.ts.logic.Faction._
import me.herbix.ts.logic.Region.Region

import scala.collection.mutable

/**
  * Created by Chaofan on 2016/6/13.
  */
class Country(val name: String, val stability: Int, val critical: Boolean, val regions: Set[Region]) {

  def this(name: String, stability: Int, critical: Boolean, region: Region) =
    this(name, stability, critical, Set(region))

  val influence = mutable.Map(US -> 0, USSR -> 0)
}
