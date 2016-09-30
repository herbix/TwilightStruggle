package me.herbix.ts.agent.simple

import me.herbix.ts.logic.{Game, Country}
import me.herbix.ts.logic.Faction.Faction
import me.herbix.ts.util.InfluenceProvider

/**
  * Created by Chaofan on 2016/9/30.
  */
class DetailInfluenceProvider(val game: Game, detail: Map[Country, Int], isAdd: Boolean, changedFaction: Faction) extends InfluenceProvider {

  override def influence(country: Country, faction: Faction): Int = {
    if (changedFaction != faction) {
      game.influence(country, faction)
    } else {
      if (isAdd) {
        game.influence(country, faction) + detail.getOrElse(country, 0)
      } else {
        game.influence(country, faction) - detail.getOrElse(country, 0)
      }
    }
  }

}
