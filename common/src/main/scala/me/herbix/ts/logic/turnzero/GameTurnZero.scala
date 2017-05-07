package me.herbix.ts.logic.turnzero

import me.herbix.ts.logic.Faction._
import me.herbix.ts.logic.Region.Region
import me.herbix.ts.logic.State.{State, _}
import me.herbix.ts.logic._
import me.herbix.ts.logic.card.{Card, Card009VietnamRevolts, Card013ArabIsraeliWar, Card023MarshallPlan}
import me.herbix.ts.logic.turnzero.TZState._
import me.herbix.ts.util._

import scala.collection.mutable

/**
  * Created by Chaofan on 2017/4/3.
  */
class GameTurnZero extends GameRecordingHistory {

  final val EuropeAlliedBerlinInfo = (3, 6, 6)

  lazy override val gameVariant = GameVariant.TurnZero

  lazy override val theCards = TZCards
  lazy override val theWorldMap = TZWorldMap

  lazy val crisisDeck = new Array[Crisis](6)
  lazy val crisisEffect = new Array[Int](6)

  var currentSolvingFlag: Flag = null

  def moveCardFromEarlyToMid(card: Card): Unit = {
    theCards.earlyWarSetValue -= card
    theCards.midWarSetValue += card
    recordHistory(new HistoryRemoveCardFromDeck(card, 1))
    recordHistory(new HistoryAddCardToDeck(card, 2))
  }

  def replaceEarlyCard(from: Card, to: Card): Unit = {
    theCards.earlyWarSetValue -= from
    theCards.earlyWarSetValue += to
    recordHistory(new HistoryRemoveCardFromDeck(from, 1))
    recordHistory(new HistoryAddCardToDeck(to, 1))
  }

  def removeCardFromGame(card: Card, period: Int): Unit = {
    if (period == 1) {
      theCards.earlyWarSetValue -= card
      recordHistory(new HistoryRemoveCardFromDeck(card, 1))
    } else if (period == 2) {
      theCards.midWarSetValue -= card
      recordHistory(new HistoryRemoveCardFromDeck(card, 2))
    }
  }

  def setTaiwanBattlefield(): Unit = {
    theWorldMap.replaceWithBattlefieldTaiwan()
    addFlag(Neutral, TZFlags.battlefieldTaiwan)
  }

  override protected def nextState(input: Operation, currentState: State): Unit = {
    currentState match {
      case TZState.chooseStateCraft => nextStateMayWait(input, nextStateChooseStateCraft)
      case TZState.solveFlags => nextStateSolveFlag(input)
      case _ => super.nextState(input, currentState)
    }
  }

  def nextStateChooseStateCraft(input: Operation): Unit = {
    val input1 = input.asInstanceOf[OperationSelectCard]
    val input2 = pendingInput.asInstanceOf[OperationSelectCard]

    val usInput = if (input1.faction == US) input1 else input2
    val ussrInput = if (input1.faction == USSR) input1 else input2

    val usCard = usInput.card.get.asInstanceOf[CardStateCraft]
    val ussrCard = ussrInput.card.get.asInstanceOf[CardStateCraft]

    recordHistory(new HistoryEvent(US, usCard))
    recordHistory(new HistoryEvent(USSR, ussrCard))

    if (!usCard.isDummy) handRemove(US, usCard)
    if (!ussrCard.isDummy) handRemove(USSR, ussrCard)

    val usModifier = if (ussrCard.cancelEffect) 0 else usCard.modifier
    val ussrModifier = if (usCard.cancelEffect) 0 else ussrCard.modifier

    val diceValue = rollDice()
    val modified = diceValue + usModifier + ussrModifier

    recordHistory(new HistoryRollDice(Neutral, diceValue, usModifier + ussrModifier))

    val ranged =
      if (modified < 1) 1
      else if (modified > 6) 6
      else modified

    crisisDeck(round - 1).effect(ranged, this)
    crisisEffect(round - 1) = ranged match {
      case 1 => 0
      case 2|3 => 1
      case 4|5 => 2
      case 6 => 3
    }

    round += 1
    pendingInput = null

    if (round == 7) {
      stateStack.pop()
      round = 0
      turn = 1
      recordHistory(new HistoryTurnRound(turn, -1, Neutral))
      hand(US).clear()
      hand(USSR).clear()

      if (hasFlagsToSolve) {
        stateStack.push(solveFlags)
        currentSolvingFlag = getFirstFlagToSolve
        operatingPlayer = TZFlags.flagsSolveFaction(currentSolvingFlag)
        prepareSolvingFlag()
      } else {
        prepareDeck()
        stateStack.push(putStartUSSR)
        operatingPlayer = USSR
      }
    } else {
      recordHistory(new HistoryTurnRound(0, round, Neutral))
      recordHistory(new HistoryCrisis(crisisDeck(round - 1)))
    }
  }

  def nextStateSolveFlag(input: Operation): Unit = {
    currentSolvingFlag match {
      case TZFlags.ussrEuropePlus1 | TZFlags.ussrEastEuropePlus2 | TZFlags.ussrMidEastPlus2 =>
        modifyInfluence(USSR, true, input.asInstanceOf[OperationModifyInfluence].detail)
      case TZFlags.ussrVietnamOrArab =>
        hand(USSR).clear()
        for (card <- input.asInstanceOf[OperationSelectCard].card) {
          handAdd(USSR, card)
          recordHistory(new HistoryGetCard(USSR, card))
        }
      case TZFlags.usMarshall =>
        hand(US).clear()
        for (card <- input.asInstanceOf[OperationSelectCard].card) {
          handAdd(US, card)
          recordHistory(new HistoryGetCard(US, card))
        }
    }

    removeFlag(US, currentSolvingFlag)
    removeFlag(USSR, currentSolvingFlag)

    if (hasFlagsToSolve) {
      currentSolvingFlag = getFirstFlagToSolve
      operatingPlayer = TZFlags.flagsSolveFaction(currentSolvingFlag)
      prepareSolvingFlag()
    } else {
      prepareDeck()
      stateStack.pop()
      stateStack.push(putStartUSSR)
      operatingPlayer = USSR
    }
  }

  def prepareSolvingFlag(): Unit = {
    currentSolvingFlag match {
      case TZFlags.ussrVietnamOrArab =>
        handAdd(USSR, Card009VietnamRevolts)
        handAdd(USSR, Card013ArabIsraeliWar)
      case TZFlags.usMarshall =>
        handAdd(US, Card023MarshallPlan)
      case _ =>
        prepareDeck()
    }
  }

  var deckPrepared: Boolean = false

  def prepareDeck(): Unit = {
    if (deckPrepared) return

    deckPrepared = true

    joinEarlyWarSet()

    pickGameStartHands(8)

    handAdd(USSR, theCards.chinaCard)
    recordHistory(new HistoryGetCard(USSR, theCards.chinaCard))
  }

  def hasFlagsToSolve: Boolean =
    TZFlags.flagsSolveList.exists(flags.hasFlag)

  def getFirstFlagToSolve: Flag =
    TZFlags.flagsSolveList.find(flags.hasFlag).orNull

  def pickStateCraftHands() = {
    TZCards.ussrStateCraft.foreach(card => handAdd(USSR, card))
    TZCards.usStateCraft.foreach(card => handAdd(US, card))
  }

  override def initGame(): Unit = {
    theCards.reset()
    theWorldMap.reset()

    pickStateCraftHands()

    modifyInfluence(USSR, true, theWorldMap.ussrStandardStart)
    modifyInfluence(US, true, theWorldMap.usStandardStart)

    operatingPlayer = Neutral

    stateStack.push(chooseStateCraft)

    val setA = shuffle(Crisis.setA.toBuffer)
    val setB = shuffle(Crisis.setB.toBuffer)
    (setA ++ setB).copyToArray(crisisDeck)

    turn = 0
    round = 1

    recordHistory(new HistoryTurnRound(0, round, Neutral))
    recordHistory(new HistoryCrisis(crisisDeck(round - 1)))
  }

  def shuffle(buffer: mutable.Buffer[Crisis]): mutable.Buffer[Crisis] = {
    def swap(a: Int, b: Int): Unit = {
      val t = buffer(a)
      buffer(a) = buffer(b)
      buffer(b) = t
    }

    for (i <- buffer.length to 2 by -1) {
      val k = random.nextInt(i)
      swap(i - 1, k)
    }

    buffer
  }

  override def beginFirstRound(): Unit = {
    round = 0
    phasingPlayer = if (!flags.hasFlag(TZFlags.usGoesFirst)) US else USSR

    nextRound()

    stateStack.push(nextActionState)
    flags.setFlagData(US, Flags.NORAD, defcon)
  }

  override def increaseRoundCounter(): Unit = {
    if (flags.hasFlag(TZFlags.usGoesFirst)) {
      if (phasingPlayer == US) {
        phasingPlayer = USSR
      } else {
        phasingPlayer = US
        round += 1
      }
    } else {
      super.increaseRoundCounter()
    }
  }

  override def nextTurn(dontTake8Rounds: Boolean = false, dontDiscardHeld: Boolean = false): Unit = {
    super.nextTurn(dontTake8Rounds, dontDiscardHeld)
    if (turn == 3) {
      removeFlag(Neutral, TZFlags.socialistGovernmentsNegated)
    }
    if (turn == 4) {
      removeFlag(Neutral, TZFlags.usGoesFirst)
    }
  }

  override def scoring(region: Region): Unit = {
    if (isEuropeAlliedBerlin && region == Region.Europe) {
      scoring(EuropeAlliedBerlinInfo, region)
    } else {
      super.scoring(region)
    }
  }

  override def setDefcon(newVal: Int): Unit = {
    val realVal = if (flags.hasFlag(TZFlags.defconLock)) Math.max(newVal, 2) else newVal
    super.setDefcon(realVal)
  }

  def isEuropeAlliedBerlin: Boolean = {
    theCards.earlyWarSet.contains(CardTZ01EuropeScoring)
  }

  override def createOperationHint(): OperationHint = {
    if (isSpectator) {
      return OperationHint.NOP
    }

    stateStack.top match {
      case TZState.chooseStateCraft =>
        OperationHint(classOf[OperationSelectCard], false, (game, card) => card.isInstanceOf[CardStateCraft])
      case TZState.solveFlags =>
        if (operatingPlayer == playerFaction) {
          currentSolvingFlag match {
            case TZFlags.ussrEuropePlus1 =>
              OperationHint(classOf[OperationModifyInfluence], 1, true, USSR,
                ConditionBuilder.influence.inRegion(Region.Europe).build, true, true, false)
            case TZFlags.ussrEastEuropePlus2 =>
              OperationHint(classOf[OperationModifyInfluence], 2, true, USSR,
                ConditionBuilder.influence.inRegion(Region.EastEurope).build, true, true, false)
            case TZFlags.ussrMidEastPlus2 =>
              OperationHint(classOf[OperationModifyInfluence], 2, true, USSR,
                ConditionBuilder.influence.inRegion(Region.MidEast).max(1).build, true, true, false)
            case TZFlags.ussrVietnamOrArab =>
              OperationHint(classOf[OperationSelectCard], true, (game, card) => true)
            case TZFlags.usMarshall =>
              OperationHint(classOf[OperationSelectCard], true, (game, card) => true)
          }
        } else {
          OperationHint.NOP
        }
      case _ => super.createOperationHint()
    }
  }

  override def nextStatePutExtra(input: Operation): Unit = {
    nextStatePutStart(input, selectHeadlineCard, Neutral)
  }

}
