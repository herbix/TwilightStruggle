// Copyright (C) 2017 Chaofan

package me.herbix.ts.logic.chinesecivilwar

import me.herbix.ts.logic.Faction.{Faction, _}
import me.herbix.ts.logic._
import me.herbix.ts.util.HistoryGetCard

/**
  * Created by Chaofan on 2017/4/3.
  */
class GameChineseCivilWar extends GameRecordingHistory {

  lazy override val theCards = CCWCards
  lazy override val gameVariant = GameVariant.ChineseCivilWar

  override protected def initGame(): Unit = {
    theWorldMap.reset()
    initGameExceptChinaCard()
    addFlag(Neutral, CCWFlags.ChineseCivilWar)
  }

  override def modifyInfluence(faction: Faction, isAdd: Boolean, detail: Map[Country, Int]): Unit = {
    super.modifyInfluence(faction, isAdd, detail)
    if (influence(theWorldMap.countryChina, USSR) >= 3) {
      removeFlag(Neutral, CCWFlags.ChineseCivilWar)
      handAdd(USSR, theCards.chinaCard)
      recordHistory(new HistoryGetCard(USSR, theCards.chinaCard))
    }
  }

  override def excludeChina(set: Set[Country]): Boolean = {
    !set(theWorldMap.countryChina) || (influence(theWorldMap.countryChina, USSR) < 3 && playerFaction == USSR)
  }

  override def excludeChina(map: Map[Country, Int]): Boolean = {
    !map.contains(theWorldMap.countryChina) || (influence(theWorldMap.countryChina, USSR) + map(theWorldMap.countryChina) <= 3 && playerFaction == USSR)
  }
}
