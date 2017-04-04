package me.herbix.ts.logic

import me.herbix.ts.logic.Region.Region

import scala.collection.mutable

/**
  * Created by Chaofan on 2016/6/13.
  */
class Country(val name: String, val stability: Int, val isBattlefield: Boolean, val regions: Set[Region])(implicit val worldMap: WorldMapTrait) {

  val id = worldMap.nextId()
  val adjacentCountries = mutable.Set.empty[Country]

  def this(name: String, stability: Int, isBattlefield: Boolean, region: Region)(implicit worldMap: WorldMapTrait) =
    this(name, stability, isBattlefield, Set(region))

  override def hashCode: Int = id
  override def equals(that: Any) = {
    if (that != null && that.isInstanceOf[Country]) that.asInstanceOf[Country].id == id else false
  }
  override def toString = name
}
