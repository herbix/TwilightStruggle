package me.herbix.ts.logic.chinacivilwar

import me.herbix.ts.logic.Faction._
import me.herbix.ts.logic._
import me.herbix.ts.logic.card._
import me.herbix.ts.util.{HistoryGetCard, HistoryLoseCard}

/**
  * Created by Chaofan on 2017/4/3.
  */
object CCWCards extends CardsTrait {
  replaceCard(Card011KoreanWar, Card011KoreanWarCCW)
  replaceCard(Card031RedScarePurge, Card031RedScarePurgeCCW)
  replaceCard(Card035Taiwan, Card035TaiwanCCW)
  replaceCard(Card058CulturalRevolution, Card058CulturalRevolutionCCW)
}

object Card011KoreanWarCCW extends CardInstant(11, 2, USSR, true) {
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    val southKorea = game.theWorldMap.countries("S.Korea")
    val modifier = southKorea.adjacentCountries.count(game.getController(_) == US)
    val modifier2 = if (game.flags.hasFlag(CCWFlags.ChineseCivilWar)) 1 else 0
    game.war(USSR, southKorea, modifier + modifier2, 4, 2, 2)
    true
  }
  override def afterPlay(game: Game, faction: Faction): Unit = {
    Card059FlowerPower.triggerFlowerPowerEffect(game, faction)
  }
}

object Card031RedScarePurgeCCW extends CardInstant(31, 4, Neutral, false) {
  override def canEvent(game: Game, faction: Faction): Boolean =
    !game.flags.hasFlag(CCWFlags.ChineseCivilWar) || faction != USSR
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    game.addFlag(Faction.getOpposite(faction), Flags.RedScarePurge)
    true
  }
}

object Card035TaiwanCCW extends CardInstant(35, 2, US, true) {
  override def canEvent(game: Game, faction: Faction): Boolean = !game.flags.hasFlag(CCWFlags.ChineseCivilWar)
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    game.addFlag(Faction.US, Flags.Taiwan)
    true
  }
}

object Card058CulturalRevolutionCCW extends CardInstant(58, 3, USSR, true) {
  override def canEvent(game: Game, faction: Faction): Boolean = !game.flags.hasFlag(CCWFlags.ChineseCivilWar)
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    val chinaCard = game.theCards.chinaCard
    if (game.hand(US).has(chinaCard)) {
      game.handRemove(US, chinaCard)
      game.handAdd(USSR, chinaCard)
      game.flags.removeFlag(US, Flags.CantPlayChinaCard)
      game.flags.removeFlag(USSR, Flags.CantPlayChinaCard)
      game.recordHistory(new HistoryLoseCard(US, chinaCard))
      game.recordHistory(new HistoryGetCard(USSR, chinaCard))
    } else {
      game.addVpAndCheck(USSR, 1)
    }
    true
  }
}

/* exclude Card071Nixon and Card076UssuriRiver because they first checks USSR has china card. */