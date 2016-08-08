package me.herbix.ts.logic

import Faction._
import me.herbix.ts.logic.card.Cards

/**
  * Created by Chaofan on 2016/8/8.
  */
class GameChineseCivilWar extends Game {

  override def modifyInfluence(faction: Faction, isAdd: Boolean, detail: Map[Country, Int]): Unit = {
    super.modifyInfluence(faction, isAdd, detail)
    if (influence(WorldMap.countryChina, USSR) >= 3) {
      removeFlag(Neutral, Flags.ChineseCivilWar)
      handAdd(USSR, Cards.chinaCard)
      recordHistory(new HistoryGetCard(USSR, Cards.chinaCard))
    }
  }

  protected override def initGame(): Unit = {
    initGameExceptChinaCard()
    addFlag(Neutral, Flags.ChineseCivilWar)
  }

  override def excludeChina(set: Set[Country]): Boolean = {
    !set(WorldMap.countryChina) || (influence(WorldMap.countryChina, USSR) < 3 && playerFaction == USSR)
  }

  override def excludeChina(map: Map[Country, Int]): Boolean = {
    !map.contains(WorldMap.countryChina) ||
      (influence(WorldMap.countryChina, USSR) + map(WorldMap.countryChina) <= 3 && playerFaction == USSR)
  }

}
