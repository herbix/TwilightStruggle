package me.herbix.ts.logic

import me.herbix.ts.logic.Faction._
import me.herbix.ts.logic.State._
import me.herbix.ts.logic.card._

/**
  * Created by Chaofan on 2016/8/8.
  */
class GameLateWar extends Game {

  protected override def initGame(): Unit = {
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

    deckJoin(Cards.earlyWarSet.filter(!Cards.isCardStarred(_)))
    deckJoin(Cards.midWarSet.filter(!Cards.isCardStarred(_)))
    deckJoin(Cards.lateWarSet)
    if (optionalCards) {
      deckJoin(Cards.earlyWarOptionalSet.filter(!Cards.isCardStarred(_)))
      deckJoin(Cards.midWarOptionalSet.filter(!Cards.isCardStarred(_)))
      deckJoin(Cards.lateWarOptionalSet)
    }
    deckAdd(Card044BearTrap)
    deckAdd(Card065CampDavidAccords)
    deckAdd(Card068JohnPaulII)
    deckAdd(Card064PanamaCanalReturned)

    pickGameStartHands(9)

    handAdd(USSR, Cards.chinaCard)
    recordHistory(new HistoryGetCard(USSR, Cards.chinaCard))

    modifyInfluence(USSR, true, WorldMap.ussrLateWarStart)
    modifyInfluence(US, true, WorldMap.usLateWarStart)

    recordHistory(new HistoryTurnRound(turn, -1, Neutral))

    stateStack.push(selectHeadlineCard)
  }

  override def endGameByVp(): Unit = {
    if (vp < 20) {
      gameOver(USSR)
    } else {
      gameOver(US)
    }
  }

}
