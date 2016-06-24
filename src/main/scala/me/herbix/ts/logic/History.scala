package me.herbix.ts.logic

import me.herbix.ts.logic.Action.Action
import me.herbix.ts.logic.Faction.Faction
import me.herbix.ts.logic.Region.Region

/**
  * Created by Chaofan on 2016/6/23.
  */
trait History

class HistoryStartGame extends History
class HistoryTurnRound(val turn: Int, val round: Int, val faction: Faction) extends History
class HistoryModifyInfluence(val faction: Faction, val isAdd: Boolean, val detail: Set[(Country, Int, Int)]) extends History
class HistoryPlayHeadline(val faction: Faction, val card: Card) extends History
class HistoryEvent(val faction: Faction, val card: Card) extends History
class HistoryCardAction(val faction: Faction, val card: Card, val action: Action, val oppositeCard: Boolean) extends History
class HistoryCardOperation(val faction: Faction, val card: Card, val operation: Action) extends History
class HistoryOperationSpace(val faction: Faction, val roll: Int) extends History
class HistoryOperationRealignment(val country: Country, val usDice: Int, val ussrDice: Int,
                                  val modifiedUsDice: Int, val modifiedUssrDice: Int) extends History
class HistoryOperationCoup(val faction: Faction, val country: Country, val dice: Int,
                           val modifier: Int, val result: Int) extends History
class HistorySpace(val faction: Faction, val oldLevel: Int, val newLevel: Int) extends History
class HistoryDefcon(val oldValue: Int, val newValue: Int) extends History
class HistoryVp(val faction: Faction, val vpChange: Int, val vpResult: Int) extends History
class HistoryMilitary(val faction: Faction, val oldValue: Int, val newValue: Int) extends History
class HistoryPickCard(val faction: Faction, val count: Int) extends History
class HistoryGetCard(val faction: Faction, val card: Card) extends History
class HistoryLoseCard(val faction: Faction, val card: Card) extends History
class HistoryDiscardCard(val faction: Faction, val card: Card) extends History
class HistoryScoring(val region: Region, val usBattle: Int, val ussrBattle: Int, val usAll: Int, val ussrAll: Int) extends History
class HistoryWar(val faction: Faction, val country: Country, val dice: Int, val result: Int) extends History
