package me.herbix.ts.logic.latewar

import me.herbix.ts.logic.Faction._
import me.herbix.ts.logic.State._
import me.herbix.ts.logic.card._
import me.herbix.ts.logic._
import me.herbix.ts.util.{HistoryTurnRound, HistoryGetCard}

/**
  * Created by Chaofan on 2017/4/3.
  */
class GameLateWar extends GameRecordingHistory {

  lazy override val gameVariant = GameVariant.LateWar

  override def initGame() = {
    turn = 8
    setDefcon(4)
    space(USSR) = SpaceLevel.Orbit
    space(US) = SpaceLevel.Landed
    addFlag(US, Flags.SpaceAwardMayDiscard)
    vp = -4

    addFlag(USSR, Flags.USJapanPact)
    addFlag(US, Flags.MarshallPlan)
    addFlag(USSR, Flags.NATO)
    addFlag(US, Flags.WarsawPact)
    addFlag(USSR, Flags.DeGaulle)
    addFlag(US, Flags.FlowerPower)

    deckJoin(theCards.earlyWarSet.filter(!theCards.isCardStarred(_)))
    deckJoin(theCards.midWarSet.filter(!theCards.isCardStarred(_)))
    deckJoin(theCards.lateWarSet)
    if (optionalCards) {
      deckJoin(theCards.earlyWarOptionalSet.filter(!theCards.isCardStarred(_)))
      deckJoin(theCards.midWarOptionalSet.filter(!theCards.isCardStarred(_)))
      deckJoin(theCards.lateWarOptionalSet)
    }
    deckAdd(Card044BearTrap)
    deckAdd(Card065CampDavidAccords)
    deckAdd(Card068JohnPaulII)
    deckAdd(Card064PanamaCanalReturned)

    pickGameStartHands(9)

    handAdd(USSR, theCards.chinaCard)
    recordHistory(new HistoryGetCard(USSR, theCards.chinaCard))

    modifyInfluence(USSR, true, theWorldMap.ussrLateWarStart)
    modifyInfluence(US, true, theWorldMap.usLateWarStart)

    recordHistory(new HistoryTurnRound(turn, -1, Neutral))

    stateStack.push(selectHeadlineCard)
  }

  override def endGameByVp() = {
    if (vp < 20) {
      gameOver(USSR)
    } else {
      gameOver(US)
    }
  }
}
