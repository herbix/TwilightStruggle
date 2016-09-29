package me.herbix.ts.util

import me.herbix.ts.logic.Action.Action
import me.herbix.ts.logic.Faction.Faction
import me.herbix.ts.logic.Region.Region
import me.herbix.ts.logic.card.Card
import me.herbix.ts.logic.{Country, Flag, Faction}

/**
  * Created by Chaofan on 2016/6/23.
  */
abstract class History {
  var id = 0
  var operatingPlayer = Faction.Neutral
  var canOperate = false
  var snapshot: Snapshot = null
}

abstract class HistoryCard(val card: Card) extends History
abstract class HistoryFlag(val faction: Faction, val flag: Flag, val data: Any) extends History
abstract class HistoryCountry(val faction: Faction, val countries: Set[Country]) extends History

class HistoryStartGame extends History
class HistoryTurnRound(val turn: Int, val round: Int, val faction: Faction) extends History
class HistoryModifyInfluence(faction: Faction, val isAdd: Boolean, val detail: Set[(Country, Int, Int)])
  extends HistoryCountry(faction, detail.map(_._1))
class HistoryPlayHeadline(val faction: Faction, card: Card) extends HistoryCard(card)
class HistoryEvent(val faction: Faction, card: Card) extends  HistoryCard(card)
class HistoryCardAction(val faction: Faction, card: Card, val action: Action, val oppositeCard: Boolean) extends HistoryCard(card)
class HistoryCardOperation(val faction: Faction, card: Card, val operation: Action) extends HistoryCard(card)
class HistoryOperationSpace(val faction: Faction, val roll: Int) extends History
class HistoryOperationRealignment(val country: Country, val usDice: Int, val ussrDice: Int,
                                  val modifiedUsDice: Int, val modifiedUssrDice: Int) extends HistoryCountry(Faction.Neutral, Set(country))
class HistoryOperationCoup(faction: Faction, val country: Country, val dice: Int,
                           val modifier: Int, val result: Int) extends HistoryCountry(faction, Set(country))
class HistorySpace(val faction: Faction, val oldLevel: Int, val newLevel: Int) extends History
class HistoryDefcon(val oldValue: Int, val newValue: Int) extends History
class HistoryVp(val faction: Faction, val vpChange: Int, val vpResult: Int) extends History
class HistoryMilitary(val faction: Faction, val oldValue: Int, val newValue: Int) extends History
class HistoryPickCard(val faction: Faction, val count: Int) extends History
class HistoryGetCard(val faction: Faction, card: Card) extends HistoryCard(card)
class HistoryLoseCard(val faction: Faction, card: Card) extends HistoryCard(card)
class HistoryDiscardCard(val faction: Faction, card: Card) extends HistoryCard(card)
class HistoryScoring(val region: Region, val usBattle: Int, val ussrBattle: Int, val usAll: Int, val ussrAll: Int) extends History
class HistoryWar(faction: Faction, val country: Country, val dice: Int, val result: Int) extends HistoryCountry(faction, Set(country))
class HistoryPokeChest(val faction: Faction) extends History
class HistoryRollDice(val faction: Faction, val dice: Int, val modifier: Int = 0) extends History
class HistoryRegion(val faction: Faction, val region: Region) extends History
class HistoryAddFlag(faction: Faction, flag: Flag, data: Any) extends HistoryFlag(faction, flag, data)
class HistoryRemoveFlag(faction: Faction, flag: Flag, data: Any) extends HistoryFlag(faction, flag, data)
class HistoryYesNo(val faction: Faction, val fromCard: Card, val value: Boolean) extends History
