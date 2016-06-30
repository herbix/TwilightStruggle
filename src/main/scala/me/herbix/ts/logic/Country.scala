package me.herbix.ts.logic

import me.herbix.ts.logic.Faction._
import me.herbix.ts.logic.Region.Region
import me.herbix.ts.util.Lang

import scala.collection.mutable

/**
  * Created by Chaofan on 2016/6/13.
  */
class Country(val name: String, val stability: Int, val isBattlefield: Boolean, val regions: Set[Region]) {

  def this(name: String, stability: Int, isBattlefield: Boolean, region: Region) =
    this(name, stability, isBattlefield, Set(region))

  val influence = mutable.Map(US -> 0, USSR -> 0)

  def getController = {
    val influenceUS = influence(US)
    val influenceUSSR = influence(USSR)
    if (influenceUS - influenceUSSR >= stability)
      US
    else if (influenceUSSR - influenceUS >= stability)
      USSR
    else
      Neutral
  }

  override def hashCode: Int = name.hashCode
  override def equals(that: Any) = {
    if (that != null && that.isInstanceOf[Country]) that.asInstanceOf[Country].name == name else false
  }
  override def toString = name
}
