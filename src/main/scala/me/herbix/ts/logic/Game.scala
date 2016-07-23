package me.herbix.ts.logic

import java.util.Random

import me.herbix.ts.logic.Faction._
import me.herbix.ts.logic.GameVariant._
import me.herbix.ts.logic.Region.Region
import me.herbix.ts.logic.State._

import scala.annotation.tailrec
import scala.collection.mutable

/**
  * Created by Chaofan on 2016/6/12.
  */
class Game extends GameTrait {

  // other players
  var anotherGame: GameTrait = null

  // player info
  var playerId = 0
  var playerFaction = Neutral
  var isSpectator = false

  // config
  var extraInfluence = 0
  var optionalCards = true
  var drawGameWinner = US
  var gameVariant = Standard

  // game info
  var randomSeed = 0l
  val random = new Random

  // game table
  val countryInfluence = WorldMap.countries.values.map(c => c -> mutable.Map(US -> 0, USSR -> 0)).toMap
  countryInfluence(WorldMap.countryUS)(US) = 100
  countryInfluence(WorldMap.countryUSSR)(USSR) = 100

  var turn = 1
  var round = 0
  var phasingPlayer = USSR

  val military = mutable.Map(US -> 0, USSR -> 0)
  val space = mutable.Map(US -> SpaceLevel(0), USSR -> SpaceLevel(0))
  var vp = 0
  var defcon = 5

  val hand = Map(US -> new CardSet, USSR -> new CardSet)
  val deck = new CardSet
  val discards = new CardSet

  val flags = new Flags

  // game states
  var pendingInput: Operation = null
  val stateStack = mutable.Stack(start)

  val operatingPlayerStack = mutable.Stack(Neutral)
  def operatingPlayer: Faction = operatingPlayerStack.top
  def operatingPlayer_=(faction: Faction): Unit = {
    if (faction != operatingPlayer) {
      clearSnapshots()
    }
    operatingPlayerStack.pop()
    operatingPlayerStack.push(faction)
  }
  def operatingPlayerChange(faction: Faction): Unit = {
    if (faction != operatingPlayer) {
      clearSnapshots()
    }
    operatingPlayerStack.push(faction)
  }
  def operatingPlayerRollBack(): Unit = {
    val oldOperatingPlayer = operatingPlayer
    operatingPlayerStack.pop()
    if (oldOperatingPlayer != operatingPlayer) {
      clearSnapshots()
    }
  }

  val currentCardStack = mutable.Stack[Card]()
  val currentCardDataStack = mutable.Stack[Any]()
  def currentCard = if (currentCardStack.isEmpty) null else currentCardStack.top
  def currentCard_=(card: Card) = {
    if (currentCardStack.nonEmpty) {
      currentCardStack.pop()
      currentCardDataStack.pop()
    }
    if (card != null) {
      currentCardStack.push(card)
      currentCardDataStack.push(null)
    }
  }
  def currentCardData = if (currentCardDataStack.isEmpty) null else currentCardDataStack.top
  def currentCardData_=(data: Any) = {
    if (currentCardDataStack.nonEmpty) {
      currentCardDataStack.pop()
      currentCardDataStack.push(data)
    }
  }
  def currentCardChange(card: Card): Unit = {
    currentCardStack.push(card)
    currentCardDataStack.push(null)
  }
  def currentCardRollBack(): Unit = {
    currentCardStack.pop()
    currentCardDataStack.pop()
  }

  var currentRealignments = List[Country]()
  var skipHeadlineCard2 = false
  // game states end

  // history
  var currentHistory = List.empty[History]
  var oldHistory = List.empty[History]
  var currentHistoryId = 0
  def history = currentHistory ++ oldHistory

  // snapshot
  var lastSnapshot = new Snapshot(this)

  // operation hint
  var currentOperationHint = createOperationHint()

  // listener
  var stateUpdateListeners: List[() => Unit] = List()

  def setRandomSeed(seed: Long): Unit = {
    randomSeed = seed
    random.setSeed(seed)
  }

  def sendNextState(input: Operation): Unit = {
    nextState(input)
    anotherGame.nextState(input)
  }

  @tailrec
  private def nextState(input: Operation, currentState: State): Unit = {
    currentState match {
      case State.start => nextStateMayWait(input, nextStateStart)
      case State.putStartUSSR => nextStatePutStart(input, putStartUS)
      case State.putStartUS => nextStatePutStartUS(input)
      case State.putStartExtra => nextStatePutExtra(input)
      case State.selectHeadlineCard => nextStateSelectHeadline(input)
      case State.selectHeadlineCard2 => nextStateSelectHeadlineNormal(input)
      case State.solveHeadLineCard1 => nextStateSolveHeadline1()
      case State.solveHeadLineCard2 => nextStateSolveHeadline2()
      case State.selectCardAndAction | State.selectAction => nextStateSelectCardAndAction(input)
      case State.cardEvent => nextStateCardEvent()
      case State.cardOperationSelect => nextStateOperationSelect(input)
      case State.cardOperationAddInfluence => nextStateOperationInfluence(input)
      case State.cardOperationRealignment => nextStateOperationRealignment(input)
      case State.cardOperationCoup => nextStateOperationCoup(input)
      case State.cardOperation => nextStateCardOperation()
      case State.discardHeldCard => nextStateDiscardHeldCard(input)
      case State.selectTake8Rounds => nextStateSelectTake8Rounds(input)
      case State.quagmireDiscard => nextStateQuagmireDiscard(input)
      case State.quagmirePlayScoringCard => nextStateQuagmireScoringCard(input)
      case State.noradInfluence => nextStateNORADInfluence(input)
      case State.cubaMissileRemove => nextStateCubaMissileRemove(input)
      case State.EventStates(n) => nextStateCardEvent(input)
    }
    checkFlags()
    if (stateStack.top == cardEventEnd) {
      stateStack.pop()
      nextState(null, stateStack.top)
    } else if (stateStack.top == cardOperation || stateStack.top == cardEventOperation || stateStack.top == cardEventAnotherCard) {
      nextState(null, stateStack.top)
    }
  }

  private def nextStateContainsException(input: Operation): Unit = {
    input match {
      case op: OperationCubaMissileRequest =>
        if (stateStack.top != cubaMissileRemove) {
          if (!op.isResponse) {
            if (op.playerId != playerId) {
              anotherGame.nextState(new OperationCubaMissileRequest(playerId, playerFaction, true))
              stateStack.push(cubaMissileRemove)
            }
          } else {
            stateStack.push(cubaMissileRemove)
          }
        }
      case _ =>
        stateStack.top match {
          case State.waitOther =>
            val top2 = stateStack(1)
            nextState(input, top2)
          case other => nextState(input, other)
        }
    }
    if (currentHistory.nonEmpty) {
      val newestHistory = currentHistory.last
      if (newestHistory.snapshot == null) {
        newestHistory.snapshot = lastSnapshot
      }
    }
    lastSnapshot = new Snapshot(this)
  }

  def nextState(input: Operation): Unit = {
    try {
      nextStateContainsException(input)
    } catch {
      case e: GameOverException =>
        operatingPlayer = e.winner
        stateStack.push(end)
    }
    currentOperationHint = createOperationHint()
    stateUpdateListeners.foreach(_())
  }

  def recordHistory(h: History): Unit = {
    h.id = currentHistoryId
    currentHistoryId += 1
    if (h.isInstanceOf[HistoryTurnRound]) {
      oldHistory = currentHistory ++ oldHistory
      currentHistory = List.empty
      clearSnapshots()
    }
    currentHistory :+= h
  }

  def addFlag(faction: Faction, flag: Flag, data: Any = null): Unit = {
    if (!flags.hasFlag(faction, flag)) {
      recordHistory(new HistoryAddFlag(faction, flag, data))
    }
    flags.addFlag(faction, flag, data)
  }

  def removeFlag(faction: Faction, flag: Flag): Unit = {
    if (flags.hasFlag(faction, flag)) {
      recordHistory(new HistoryRemoveFlag(faction, flag, flags.getFlagData(faction, flag)))
    }
    flags.removeFlag(faction, flag)
  }

  def handAdd(faction: Faction, card: Card): Unit = {
    hand(faction).add(card)
  }

  def handRemove(faction: Faction, card: Card): Unit = {
    hand(faction).remove(card)
  }

  def discardsAdd(card: Card): Unit = {
    discards.add(card)
  }

  def discardsRemove(card: Card): Unit = {
    discards.remove(card)
  }

  def discardsClear(): Unit = {
    discards.clear()
  }

  private def nextStateMayWait(input: Operation, nextStateReal: Operation => Unit): Unit = {
    if (pendingInput == null) {
      pendingInput = input
      if (input.playerId == playerId) {
        stateStack.push(waitOther)
      }
    } else {
      if (stateStack.top == waitOther) {
        stateStack.pop()
      }
      nextStateReal(input)
    }
  }

  def nextStateStart(input: Operation): Unit = {
    val input1 = input.asInstanceOf[OperationChooseFaction]
    val input2 = pendingInput.asInstanceOf[OperationChooseFaction]

    if (input1.playerId != playerId && input2.playerId != playerId) {
      isSpectator = true
    }

    val myInput = if (input1.playerId == playerId) input1 else input2
    val smallInput = if (input1.playerId < input2.playerId) input1 else input2

    if (input1.faction != input2.faction) {
      playerFaction = myInput.faction
    } else {
      val smallChange = random.nextBoolean()
      if (smallChange == (smallInput == myInput)) {
        playerFaction = if (myInput.faction == US) USSR else US
      } else {
        playerFaction = myInput.faction
      }
    }

    pendingInput = null

    recordHistory(new HistoryStartGame)

    stateStack.pop()

    initGame()

    if (isSpectator) {
      playerFaction = Neutral
    }
  }

  def nextStatePutExtra(input: Operation): Unit = {
    nextStatePutStart(input, selectHeadlineCard)
    recordHistory(new HistoryTurnRound(turn, -1, Neutral))
  }

  def pickCardFromDeck(): Card = {
    if (deck.cardCount == 0) {
      deckJoin(discards)
      discardsClear()
    }
    clearSnapshots()
    deck.pickAndRemove(random)
  }

  def deckAdd(card: Card): Unit = {
    deck.add(card)
  }

  def deckJoin(set: Iterable[Card]): Unit = {
    deck.join(set)
  }

  private def initGame(): Unit = {
    gameVariant match {
      case GameVariant.Standard => initGameStandard()
      case GameVariant.ChinaCivilWar => initGameExceptChinaCard()
      case GameVariant.LateWar => initGameLateWar()
    }
  }

  def initGameStandard(): Unit = {
    initGameExceptChinaCard()

    handAdd(USSR, Cards.chinaCard)
    recordHistory(new HistoryGetCard(USSR, Cards.chinaCard))
  }

  def initGameExceptChinaCard(): Unit = {
    deckJoin(Cards.earlyWarSet)
    if (optionalCards) {
      deckJoin(Cards.earlyWarOptionalSet)
    }

    pickGameStartHands(8)

    modifyInfluence(USSR, true, WorldMap.ussrStandardStart)
    modifyInfluence(US, true, WorldMap.usStandardStart)

    stateStack.push(putStartUSSR)
  }

  def pickGameStartHands(count: Int): Unit = {
    for (i <- 0 until count) {
      handAdd(US, pickCardFromDeck())
      handAdd(USSR, pickCardFromDeck())
    }
    recordHistory(new HistoryPickCard(US, 8))
    recordHistory(new HistoryPickCard(USSR, 8))
  }

  def initGameLateWar(): Unit = {
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

  def nextStatePutStartUS(input: Operation): Unit = {
    if (extraInfluence != 0) {
      nextStatePutStart(input, putStartExtra)
    } else {
      nextStatePutExtra(input)
    }
  }

  def nextStatePutStart(input: Operation, next: State): Unit = {
    val op = input.asInstanceOf[OperationModifyInfluence]
    modifyInfluence(op.faction, op.isAdd, op.detail)
    stateStack.pop()
    stateStack.push(next)
  }

  def influence(country: Country, faction: Faction): Int = {
    countryInfluence(country)(faction)
  }

  def calculateInfluenceCost(pendingInfluenceChange: mutable.Map[Country, Int], faction: Faction, ignoreControl: Boolean): Int = {
    var cost = 0
    for ((country, modifyValue) <- pendingInfluenceChange) {
      val influenceSelf = influence(country, faction)
      val c =
        if (ignoreControl)
          modifyValue
        else {
          val influenceOpposite = influence(country, Faction.getOpposite(faction))
          if (influenceOpposite - influenceSelf >= country.stability) {
            modifyValue + Math.min(modifyValue, influenceOpposite - influenceSelf - country.stability + 1)
          } else {
            modifyValue
          }
        }
      cost += c
    }
    cost
  }

  def modifyInfluence(faction: Faction, isAdd: Boolean, detail: Map[Country, Int]): Unit = {
    var detail2 = Set[(Country, Int, Int)]()
    for ((country, value) <- detail) {
      val oldInfluence = influence(country, faction)
      val newInfluence = Math.max(0, oldInfluence + (if (isAdd) value else -value))
      countryInfluence(country)(faction) = newInfluence
      if (oldInfluence != newInfluence) {
        detail2 += ((country, oldInfluence, newInfluence))
      }
    }
    if (detail2.nonEmpty) {
      recordHistory(new HistoryModifyInfluence(faction, isAdd, detail2))
    }
  }

  def discardCard(card: Card, from: Faction, force: Boolean = false, showInHistory: Boolean = false): Unit = {
    if (showInHistory) {
      recordHistory(new HistoryDiscardCard(from, card))
    }
    if (card == Cards.chinaCard) {
      val opposite = Faction.getOpposite(from)
      handAdd(opposite, card)
      recordHistory(new HistoryGetCard(opposite, card))
      addFlag(opposite, Flags.CantPlayChinaCard)
    } else if (force || !card.isRemovedAfterEvent) {
      discardsAdd(card)
    }
  }

  def nextActionState: State = {
    if (flags.hasFlag(phasingPlayer, Flags.QuagmireBearTrap)) {
      val scoringCardCount = hand(phasingPlayer).count(!_.canHeld(this))
      if (hand(phasingPlayer).exists(card => card.canHeld(this) && card.canPlay(this, phasingPlayer)) &&
        scoringCardCount < turnRoundCount + 1 - round) {
        quagmireDiscard
      } else {
        quagmirePlayScoringCard
      }
    } else {
      selectCardAndAction
    }
  }

  def nextStateSelectHeadline(input: Operation): Unit = {
    if (!flags.hasFlag(Flags.SpaceAwardHeadlineThen))
      nextStateMayWait(input, nextStateSelectHeadlineNormal)
    else
      nextStateSelectHeadlineFirst(input)
  }

  def nextStateSelectHeadlineFirst(input: Operation): Unit = {
    val op = input.asInstanceOf[OperationSelectCard]
    pendingInput = input
    recordHistory(new HistoryPlayHeadline(op.faction, op.card.get))
    stateStack.pop()
    stateStack.push(selectHeadlineCard2)
  }

  def nextStateSelectHeadlineNormal(input: Operation): Unit = {
    val input1 = input.asInstanceOf[OperationSelectCard]
    val input2 = pendingInput.asInstanceOf[OperationSelectCard]

    val input1Card = input1.card.get
    val input2Card = input2.card.get

    val inputA = if (
      (
        (input1Card.op > input2Card.op || (input1Card.op == input2Card.op && input1.faction == US)) &&
        !(input2.faction == US && input2Card == Card103Defectors)
      ) || (input1.faction == US && input1Card == Card103Defectors)
    ) input1 else input2
    val inputB = if (input1 == inputA) input2 else input1

    val inputACard = inputA.card.get
    val inputBCard = inputB.card.get

    handRemove(inputA.faction, inputACard)
    handRemove(inputB.faction, inputBCard)

    discardCard(inputACard, inputA.faction)
    discardCard(inputBCard, inputB.faction)

    if (stateStack.top == selectHeadlineCard2) {
      recordHistory(new HistoryPlayHeadline(input1.faction, input1Card))
    } else {
      recordHistory(new HistoryPlayHeadline(inputA.faction, inputACard))
      recordHistory(new HistoryPlayHeadline(inputB.faction, inputBCard))
    }

    currentCard = inputACard
    phasingPlayer = inputA.faction
    operatingPlayer = inputACard.getOperatingPlayer(inputA.faction)

    pendingInput = inputB

    stateStack.pop()
    stateStack.push(solveHeadLineCard1)

    skipHeadlineCard2 = false

    recordHistory(new HistoryTurnRound(turn, round, phasingPlayer))
    recordHistory(new HistoryEvent(operatingPlayer, currentCard))

    stateStack.push(cardEventStart)
    currentCard.nextState(this, operatingPlayer, null)
  }

  def nextStateSolveHeadline1(): Unit = {
    currentCard.afterPlay(this, operatingPlayer)

    val input = pendingInput.asInstanceOf[OperationSelectCard]

    if (skipHeadlineCard2) {
      discardCard(input.card.get, input.faction, true)
      pendingInput = null
      currentCard = null

      stateStack.pop()
      beginFirstRound()
      return
    }

    currentCard = input.card.get
    phasingPlayer = input.faction
    operatingPlayer = currentCard.getOperatingPlayer(input.faction)

    pendingInput = null

    recordHistory(new HistoryTurnRound(turn, round, phasingPlayer))

    stateStack.pop()
    stateStack.push(solveHeadLineCard2)

    recordHistory(new HistoryEvent(operatingPlayer, currentCard))

    stateStack.push(cardEventStart)
    currentCard.nextState(this, operatingPlayer, null)

  }

  def nextStateSolveHeadline2() = {
    currentCard.afterPlay(this, operatingPlayer)
    currentCard = null

    stateStack.pop()
    beginFirstRound()
  }

  def beginFirstRound(): Unit = {
    round = 1
    phasingPlayer = USSR
    operatingPlayer = phasingPlayer

    stateStack.push(nextActionState)
    flags.setFlagData(US, Flags.NORAD, defcon)

    recordHistory(new HistoryTurnRound(turn, round, phasingPlayer))
  }

  @tailrec
  private def nextRound(): Boolean = {
    if (phasingPlayer == USSR) {
      phasingPlayer = US
    } else {
      phasingPlayer = USSR
      round += 1
    }
    operatingPlayer = phasingPlayer
    if (hand(phasingPlayer).canPlayCardCount(this, phasingPlayer) == 0 && !isAfterFinalRound) {
      nextRound()
    } else {
      if (!isAfterFinalRound) {
        recordHistory(new HistoryTurnRound(turn, round, phasingPlayer))
        true
      } else {
        false
      }
    }
  }

  def isAfterFinalRound = round > turnRoundCount
  def turnRoundCount = if (turn <= 3) 6 else 7

  def rollDice() = {
    clearSnapshots()
    random.nextInt(6) + 1
  }

  def increaseSpace(faction: Faction, value: Int) = {
    val oldSpace = space(faction)
    var selfSpace = oldSpace
    val oppositeFaction = Faction.getOpposite(faction)

    for (i <- 0 until value) {
      val nextLevel = selfSpace.nextLevel
      if (nextLevel != null) {
        selfSpace = nextLevel
        if (selfSpace.flag != null) {
          if (flags.hasFlag(oppositeFaction, selfSpace.flag)) {
            removeFlag(oppositeFaction, selfSpace.flag)
          } else {
            addFlag(faction, selfSpace.flag)
          }
        }
      }
    }
    space(faction) = selfSpace

    recordHistory(new HistorySpace(faction, oldSpace.level, selfSpace.level))

    val oppositeSpace = space(oppositeFaction)
    addVpAndCheck(faction, if (oppositeSpace.level < selfSpace.level) selfSpace.firstVp else selfSpace.secondVp)
  }

  def addVp(faction: Faction, value: Int): Unit = {
    vp += Faction.getVpFactor(faction) * value
    if (value != 0) {
      recordHistory(new HistoryVp(faction, value, vp))
    }
  }

  def checkVp(): Unit = {
    if (vp >= 20) {
      vp = 20
      gameOver(US)
    }
    if (vp <= -20) {
      vp = -20
      gameOver(USSR)
    }
  }

  def addVpAndCheck(faction: Faction, value: Int): Unit = {
    addVp(faction, value)
    checkVp()
  }

  def setDefcon(newVal: Int): Unit = {
    val oldVal = defcon
    defcon = newVal
    recordHistory(new HistoryDefcon(oldVal, defcon))
    checkDefcon()
  }

  def mayTake8Rounds(faction: Faction) =
    (flags.hasFlag(faction, Flags.SpaceAwardTake8Rounds) || flags.hasFlag(faction, Flags.NorthSeaOil8Rounds)) &&
    hand(faction).canPlayCardCount(this, faction) > 0

  def nextTurn(dontTake8Rounds: Boolean = false, dontDiscardHeld: Boolean = false): Unit = {
    if (round == turnRoundCount + 1 && !dontTake8Rounds) {
      if (mayTake8Rounds(phasingPlayer)) {
        stateStack.push(selectTake8Rounds)
        return
      }
      nextRound()
      if (round == turnRoundCount + 1 && mayTake8Rounds(phasingPlayer)) {
        stateStack.push(selectTake8Rounds)
        return
      }
    }

    val usFail = hand(US).exists(!_.canHeld(this))
    val ussrFail = hand(USSR).exists(!_.canHeld(this))

    if (usFail != ussrFail) {
      if (usFail) gameOver(USSR) else gameOver(US)
    } else if (usFail) {
      gameOver(Neutral)
    }

    if (flags.hasFlag(Flags.SpaceAwardMayDiscard) && !dontDiscardHeld) {
      val faction = if (flags.hasFlag(US, Flags.SpaceAwardMayDiscard)) US else USSR
      if (hand(faction).exists(_.canDiscard(this, faction))) {
        if (round > 8) {
          round = 8
          phasingPlayer = US
        }
        stateStack.push(discardHeldCard)
        return
      }
    }

    turn += 1
    if (turn > 10) {
      finalScoring()
      endGameByVp()
      return
    }
    round = 0

    recordHistory(new HistoryTurnRound(turn, -1, Neutral))

    if (turn == 4) {
      deckJoin(Cards.midWarSet)
      if (optionalCards) {
        deckJoin(Cards.midWarOptionalSet)
      }
    } else if (turn == 8) {
      deckJoin(Cards.lateWarSet)
      if (optionalCards) {
        deckJoin(Cards.lateWarOptionalSet)
      }
    }

    val usHandCount = hand(US).cardCount
    val ussrHandCount = hand(USSR).cardCount

    val handCount = turnRoundCount + 2
    for (i <- 0 until handCount) {
      if (hand(US).cardCountExcludingChinaCard < handCount) {
        handAdd(US, pickCardFromDeck())
      }
      if (hand(USSR).cardCountExcludingChinaCard < handCount) {
        handAdd(USSR, pickCardFromDeck())
      }
    }

    recordHistory(new HistoryPickCard(US, hand(US).cardCount - usHandCount))
    recordHistory(new HistoryPickCard(USSR, hand(USSR).cardCount - ussrHandCount))

    if (military(US) < defcon) {
      addVp(USSR, defcon - military(US))
    }

    if (military(USSR) < defcon) {
      addVp(US, defcon - military(USSR))
    }

    checkVp()

    setMilitary(US, 0)
    setMilitary(USSR, 0)
    setDefcon(defcon + 1)

    flags.turnEnds()

    stateStack.push(selectHeadlineCard)
  }

  def tryNextRound(): Unit = {
    if (stateStack.isEmpty) {
      if (flags.hasFlag(US, Flags.NORAD) && WorldMap.countries("Canada").getController(this) == US &&
        flags.getFlagData(US, Flags.NORAD) != defcon && defcon == 2) {
        stateStack.push(noradInfluence)
      } else if (nextRound()) {
        stateStack.push(nextActionState)
        flags.setFlagData(US, Flags.NORAD, defcon)
      } else {
        nextTurn()
      }
    }
  }

  def nextStateSelectCardAndAction(input: Operation): Unit = {
    val op = input.asInstanceOf[OperationSelectCardAndAction]
    val oppositeCard = op.card.faction == Faction.getOpposite(op.faction)
    val alwaysTriggerEvent = oppositeCard && op.card.canEvent(this, op.faction)

    handRemove(op.faction, op.card)

    currentCard = op.card

    recordHistory(new HistoryCardAction(op.faction, op.card, op.action, oppositeCard))

    if (flags.hasFlag(op.faction, Flags.WeWillBuryYou)) {
      if (op.action != Action.Event || op.card != Card032UNIntervention) {
        addVpAndCheck(Faction.getOpposite(op.faction), 3)
      }
      removeFlag(op.faction, Flags.WeWillBuryYou)
    }

    op.action match {
      case Action.Space =>
        discardCard(op.card, op.faction, true)
        val roll = rollDice()
        recordHistory(new HistoryOperationSpace(op.faction, roll))
        if (roll <= space(op.faction).nextLevel.rollMax) {
          increaseSpace(op.faction, 1)
        }
        if (flags.hasFlag(op.faction, Flags.Space1)) {
          addFlag(op.faction, Flags.Space2)
        } else {
          addFlag(op.faction, Flags.Space1)
        }
        currentCard.afterPlay(this, op.faction)
        currentCard = null
        stateStack.pop()
        tryNextRound()
      case Action.Event =>
        discardCard(op.card, op.faction)
        stateStack.pop()
        if (!oppositeCard) {
          stateStack.push(cardE)
        } else {
          stateStack.push(cardEO)
        }
        operatingPlayerChange(currentCard.getOperatingPlayer(operatingPlayer))
        currentCardChange(currentCard)
        recordHistory(new HistoryEvent(operatingPlayer, currentCard))
        stateStack.push(cardEvent)
        stateStack.push(cardEventStart)
        currentCard.nextState(this, operatingPlayer, null)
      case Action.Operation =>
        discardCard(op.card, op.faction, !alwaysTriggerEvent)
        stateStack.pop()
        if (!alwaysTriggerEvent) {
          stateStack.push(cardO)
        } else {
          stateStack.push(cardOE)
        }
        stateStack.push(cardOperation)
        stateStack.push(cardOperationSelect)
    }

  }

  def canSpace(faction: Faction): Boolean = {
    space(faction) != SpaceLevel.Station && !flags.hasFlag(faction, Flags.MissileEnvy) &&
      (!flags.hasFlag(faction, Flags.Space1) ||
        (!flags.hasFlag(faction, Flags.Space2) && flags.hasFlag(faction, Flags.SpaceAwardTwoSpace)))
  }

  def canCardSpace(card: Card, faction: Faction): Boolean = {
    card.id != 0 && canSpace(faction) && space(faction).nextLevel.op <= modifyOp(faction, card.op)
  }

  def canCardEvent(card: Card, faction: Faction): Boolean = {
    card.id != 0 && card.canEvent(this, faction)
  }

  def canCardOperation(card: Card, faction: Faction): Boolean = {
    card.id != 0 && card.op > 0
  }

  def nextStateCardEvent() = {
    operatingPlayerRollBack()
    currentCardRollBack()
    stateStack.pop()
    stateStack.top match {
      case State.cardEO =>
        stateStack.push(cardOperation)
        stateStack.push(cardOperationSelect)
      case State.cardOE | State.cardE =>
        currentCard.afterPlay(this, operatingPlayer)
        currentCard = null
        stateStack.pop()
        tryNextRound()
    }
  }

  def nextStateCardEvent(input: Operation) = {
    stateStack.pop()
    currentCard.nextState(this, operatingPlayer, input)
  }

  def nextStateOperationSelect(input: Operation) = {
    val op = input.asInstanceOf[OperationSelectOperation]
    stateStack.pop()

    recordHistory(new HistoryCardOperation(op.faction, currentCard, op.action))

    op.action match {
      case Action.Influence =>
        stateStack.push(cardOperationAddInfluence)
      case Action.Realignment =>
        stateStack.push(cardOperationRealignment)
        currentRealignments = List.empty
      case Action.Coup =>
        stateStack.push(cardOperationCoup)
    }
  }

  def canAddInfluence(faction: Faction, country: Country): Boolean = {
    if (flags.hasFlag(faction, Flags.Chernobyl) &&
      country.regions(flags.getFlagData(faction, Flags.Chernobyl).asInstanceOf[Region])) {
      return false
    }
    influence(country, faction) > 0 ||
      country.adjacentCountries.exists(influence(_, faction) > 0)
  }

  def canAddInfluence(faction: Faction)(targets: Map[Country, Int]): Boolean = {
    targets.forall(e => canAddInfluence(faction, e._1))
  }

  def nextStateOperationInfluence(input: Operation) = {
    val op = input.asInstanceOf[OperationModifyInfluence]
    modifyInfluence(op.faction, op.isAdd, op.detail)
    stateStack.pop()
  }

  def canRealignment(faction: Faction, country: Country): Boolean = {
    if (country.regions(Region.Special)) return false
    if (modifyOp(faction, currentCard.op, currentRealignments :+ country) - currentRealignments.size - 1 < 0) {
      return false
    }
    for (Some(can) <- flags.flagSets2(faction).toStream.map(_.canRealignment(this, country)).find(_.isDefined)) {
      return can
    }
    true
  }

  def canCoup(faction: Faction, country: Country): Boolean = {
    if (country.regions(Region.Special)) return false
    if (influence(country, Faction.getOpposite(faction)) <= 0) {
      return false
    }
    for (Some(can) <- flags.flagSets2(faction).toStream.map(_.canCoup(this, country)).find(_.isDefined)) {
      return can
    }
    true
  }

  def canCoup(faction: Faction, filter: Country => Boolean = _ => true): Boolean = {
    WorldMap.countries.exists(e => filter(e._2) && canCoup(faction, e._2))
  }

  def canCoupWithoutFlags(faction: Faction, country: Country): Boolean = {
    !country.regions(Region.Special) && influence(country, Faction.getOpposite(faction)) > 0
  }

  def canCoupWithoutFlags(faction: Faction, filter: Country => Boolean = _ => true): Boolean = {
    WorldMap.countries.exists(e => filter(e._2) && canCoupWithoutFlags(faction, e._2))
  }

  def getCurrentRealignmentRest(faction: Faction): Int =
    modifyOp(faction, currentCard.op, currentRealignments) - currentRealignments.size

  def nextStateOperationRealignment(input: Operation) = {
    val op = input.asInstanceOf[OperationSelectCountry]
    val country = op.detail.head
    realignment(country)
    currentRealignments = currentRealignments :+ country
    if (getCurrentRealignmentRest(op.faction) <= 0) {
      stateStack.pop()
    }
  }

  def realignment(country: Country): Unit = {
    val rollUSOriginal = rollDice()
    val rollUSSROriginal = rollDice()
    var rollUS = rollUSOriginal
    var rollUSSR = rollUSSROriginal
    if (influence(country, US) > influence(country, USSR)) {
      rollUS += 1
    } else if (influence(country, US) < influence(country, USSR)) {
      rollUSSR += 1
    }
    rollUS += country.adjacentCountries.count(_.getController(this) == US)
    rollUSSR += country.adjacentCountries.count(_.getController(this) == USSR)
    if (flags.hasFlag(US, Flags.IranContra)) {
      rollUS -= 1
    }
    recordHistory(new HistoryOperationRealignment(country, rollUSOriginal, rollUSSROriginal, rollUS, rollUSSR))
    if (rollUS > rollUSSR) {
      val decreaseValue = Math.min(rollUS - rollUSSR, influence(country, USSR))
      if (decreaseValue > 0) modifyInfluence(USSR, false, Map(country -> decreaseValue))
    } else if (rollUS < rollUSSR) {
      val decreaseValue = Math.min(rollUSSR - rollUS, influence(country, US))
      if (decreaseValue > 0) modifyInfluence(US, false, Map(country -> decreaseValue))
    }
  }

  def checkDefcon() = {
    if (defcon <= 1) {
      defcon = 1
      gameOver(Faction.getOpposite(phasingPlayer))
    }
    if (defcon > 5) {
      defcon = 5
    }
    if (defcon <= 4) addFlag(Neutral, Flags.Defcon4Penalty) else removeFlag(Neutral, Flags.Defcon4Penalty)
    if (defcon <= 3) addFlag(Neutral, Flags.Defcon3Penalty) else removeFlag(Neutral, Flags.Defcon3Penalty)
    if (defcon <= 2) addFlag(Neutral, Flags.Defcon2Penalty) else removeFlag(Neutral, Flags.Defcon2Penalty)
  }

  def addMilitary(faction: Faction, value: Int) = setMilitary(faction, military(faction) + value)

  def setMilitary(faction: Faction, value: Int) = {
    val oldValue = military(faction)
    military(faction) = value
    if (military(faction) > 5) {
      military(faction) = 5
    }
    if (military(faction) < 0) {
      military(faction) = 0
    }
    if (oldValue != military(faction)) {
      recordHistory(new HistoryMilitary(faction, oldValue, military(faction)))
    }
  }

  def nextStateOperationCoup(input: Operation) = {
    val op = input.asInstanceOf[OperationSelectCountry]
    val country = op.detail.head
    val faction = op.faction
    val modifiedOp = modifyOp(faction, currentCard.op, Set(country))

    coup(country, faction, modifiedOp)
    addMilitary(faction, modifiedOp)

    stateStack.pop()
  }

  def coup(country: Country, faction: Faction, op: Int): Int = {
    val rollResult = rollDice()
    val modifierSalt = if (flags.hasFlag(Flags.SALT)) -1 else 0
    val modifierDeathSquads = if (flags.hasFlag(faction, Flags.DeathSquads)) 1 else
      if (flags.hasFlag(faction, Flags.DeathSquads2)) -1 else 0
    val coupFactor = Math.max(0, rollResult + op - 2 * country.stability + modifierSalt + modifierDeathSquads)

    recordHistory(new HistoryOperationCoup(faction, country, rollResult, op, coupFactor))

    val factionSelf = faction
    val factionOpposite = Faction.getOpposite(factionSelf)

    val oppositeDown = Math.min(coupFactor, influence(country, factionOpposite))
    val selfUp = coupFactor - oppositeDown

    if (oppositeDown > 0) {
      modifyInfluence(factionOpposite, false, Map(country -> oppositeDown))
    }
    if (selfUp > 0) {
      modifyInfluence(factionSelf, true, Map(country -> selfUp))
    }

    if (country.isBattlefield && !flags.hasFlag(faction, Flags.NuclearSubs)) {
      setDefcon(defcon - 1)
    }

    if (flags.hasFlag(faction, Flags.CubaMissile)) {
      gameOver(Faction.getOpposite(faction))
    }

    if (flags.hasFlag(Faction.getOpposite(faction), Flags.Samantha)) {
      addVpAndCheck(Faction.getOpposite(faction), 1)
    }

    coupFactor
  }

  def nextStateCardOperation() = {
    stateStack.pop()
    stateStack.top match {
      case State.cardOE =>
        operatingPlayerChange(currentCard.getOperatingPlayer(operatingPlayer))
        currentCardChange(currentCard)
        recordHistory(new HistoryEvent(operatingPlayer, currentCard))
        stateStack.push(cardEvent)
        stateStack.push(cardEventStart)
        currentCard.nextState(this, operatingPlayer, null)
      case State.cardEO | State.cardO =>
        currentCard.afterPlay(this, operatingPlayer)
        currentCard = null
        stateStack.pop()
        tryNextRound()
    }
  }

  def nextStateDiscardHeldCard(input: Operation) = {
    val op = input.asInstanceOf[OperationSelectCard]

    for (card <- op.card) {
      handRemove(op.faction, card)
      discardCard(card, op.faction, true, true)
    }

    stateStack.pop()
    nextTurn(true, true)
  }

  def nextStateSelectTake8Rounds(input: Operation) = {
    val op = input.asInstanceOf[OperationYesNo]

    stateStack.pop()
    if (op.value) {
      stateStack.push(nextActionState)
      recordHistory(new HistoryTurnRound(turn, round, phasingPlayer))
    } else {
      nextRound()
      nextTurn()
    }
  }

  def modifyOp(faction: Faction, originalOp: Int, targets: Iterable[Country]): Int = {
    var modifiedOp = modifyOp(faction, originalOp)
    if (currentCard != null) {
      modifiedOp = currentCard.modifyOp(this, faction, modifiedOp, targets)
    }
    if (flags.hasFlag(faction, Flags.VietnamRevolts) && targets.forall(_.regions(Region.SouthEastAsia))) {
      modifiedOp += 1
    }
    modifiedOp
  }

  def modifyOp(faction: Faction, originalOp: Int): Int = {
    var modifiedOp = originalOp
    if (flags.hasFlag(faction, Flags.Containment)) {
      modifiedOp += 1
    }
    if (flags.hasFlag(faction, Flags.RedScarePurge)) {
      modifiedOp -= 1
    }
    if (modifiedOp > 4) modifiedOp = 4
    if (modifiedOp < 1) modifiedOp = 1
    if (currentCard != null) {
      modifiedOp = currentCard.modifyOp(this, faction, modifiedOp)
    }
    modifiedOp
  }

  def war(faction: Faction, country: Country, modifier: Int, minRoll: Int, military: Int, vp: Int) = {
    val dice = rollDice()
    val modified = dice - modifier
    addMilitary(faction, military)
    recordHistory(new HistoryWar(faction, country, dice, modified))
    if (modified >= minRoll) {
      addVpAndCheck(faction, vp)
      val theInfluence = influence(country, Faction.getOpposite(faction))
      modifyInfluence(Faction.getOpposite(faction), false, Map(country -> theInfluence))
      modifyInfluence(faction, true, Map(country -> theInfluence))
    }
  }

  def scoring(region: Region): Unit = {
    val info = Region.ScoringInfo(region)
    val presence = info._1
    val domination = info._2
    val control = info._3

    val targetCountries = WorldMap.regionCountries(region)
    val battlefieldCount = targetCountries.count(_.isBattlefield)
    var usBattlefield = targetCountries.count(country => country.isBattlefield && country.getController(this) == US)
    val usNonBattlefield = targetCountries.count(country => !country.isBattlefield && country.getController(this) == US)
    var ussrBattlefield = targetCountries.count(country => country.isBattlefield && country.getController(this) == USSR)
    val ussrNonBattlefield = targetCountries.count(country => !country.isBattlefield && country.getController(this) == USSR)
    val usAll = usBattlefield + usNonBattlefield
    val ussrAll = ussrBattlefield + ussrNonBattlefield

    val taiwan = WorldMap.countries("Taiwan")
    if (taiwan.regions(region) && taiwan.getController(this) == US && flags.hasFlag(US, Flags.Taiwan)) {
      usBattlefield += 1
    }

    val useShuttleDiplomacy = flags.hasFlag(Flags.ShuttleDiplomacy) && (region == Region.Asia || region == Region.MidEast)
    if (useShuttleDiplomacy) {
      ussrBattlefield -= 1
    }

    val usPresence = usBattlefield > 0 || usNonBattlefield > 0
    val ussrPresence = ussrBattlefield > 0 || ussrNonBattlefield > 0
    val usDomination = usBattlefield > ussrBattlefield && usAll > ussrAll && usNonBattlefield > 0
    val ussrDomination = ussrBattlefield > usBattlefield && ussrAll > usAll && ussrNonBattlefield > 0
    val usControl = usBattlefield == battlefieldCount && usAll > ussrAll
    val ussrControl = ussrBattlefield == battlefieldCount && ussrAll > usAll

    var usVp = if (usControl) control else if (usDomination) domination else if (usPresence) presence else 0
    var ussrVp = if (ussrControl) control else if (ussrDomination) domination else if (ussrPresence) presence else 0

    usVp += usBattlefield
    ussrVp += ussrBattlefield

    usVp += targetCountries.count(country => country.getController(this) == US && country.adjacentCountries(WorldMap.countryUSSR))
    ussrVp += targetCountries.count(country => country.getController(this) == USSR && country.adjacentCountries(WorldMap.countryUS))

    if (useShuttleDiplomacy) {
      ussrVp -= (if (targetCountries.exists(country => {
        country.getController(this) == USSR && country.adjacentCountries(WorldMap.countryUS) && country.isBattlefield
      })) 1 else 0)
    }

    recordHistory(new HistoryScoring(region, usBattlefield, ussrBattlefield, usAll, ussrAll))

    addVp(US, usVp)
    addVp(USSR, ussrVp)

    if (useShuttleDiplomacy) {
      flags.removeFlag(US, Flags.ShuttleDiplomacy)
      discardCard(Card073ShuttleDiplomacy, US, true)
    }
  }

  def finalScoring(): Unit = {
    flags.removeFlag(US, Flags.ShuttleDiplomacy)
    scoring(Region.Asia)
    scoring(Region.Europe)
    scoring(Region.MidEast)
    scoring(Region.Africa)
    scoring(Region.MidAmerica)
    scoring(Region.SouthAmerica)
    checkVp()
  }

  def gameOver(faction: Faction): Unit = {
    if (faction == Neutral) {
      throw new GameOverException(drawGameWinner)
    } else {
      throw new GameOverException(faction)
    }
  }

  def pokeChest(faction: Faction) = {
    recordHistory(new HistoryPokeChest(faction))
  }

  def endGameByVp(): Unit = {
    gameVariant match {
      case GameVariant.LateWar => endGameByVpLateWar()
      case _ => endGameByVpStandard()
    }
  }

  def endGameByVpStandard(): Unit = {
    if (vp < 0) {
      gameOver(USSR)
    } else if (vp > 0) {
      gameOver(US)
    } else {
      gameOver(Neutral)
    }
  }

  def endGameByVpLateWar(): Unit = {
    if (vp < 20) {
      gameOver(USSR)
    } else {
      gameOver(US)
    }
  }

  def nextStateQuagmireDiscard(input: Operation) = {
    val op = input.asInstanceOf[OperationSelectCard]
    val card = op.card.get

    handRemove(op.faction, card)
    discardCard(card, op.faction, true, true)

    val dice = rollDice()
    if (dice <= 4) {
      removeFlag(op.faction, Flags.QuagmireBearTrap)
    }
    recordHistory(new HistoryRollDice(op.faction, dice))

    stateStack.pop()
    if (nextRound()) {
      stateStack.push(nextActionState)
    } else {
      nextTurn()
    }
  }

  def nextStateQuagmireScoringCard(input: Operation) = {
    val op = input.asInstanceOf[OperationSelectCard]
    val card = op.card.get

    handRemove(op.faction, card)
    discardCard(card, op.faction)

    recordHistory(new HistoryCardAction(op.faction, card, Action.Event, false))

    currentCard = card

    stateStack.pop()
    stateStack.push(cardE)

    recordHistory(new HistoryEvent(operatingPlayer, currentCard))
    stateStack.push(cardEvent)
    stateStack.push(cardEventStart)
    currentCard.nextState(this, operatingPlayer, null)
  }

  def nextStateNORADInfluence(input: Operation): Unit = {
    val op = input.asInstanceOf[OperationModifyInfluence]

    modifyInfluence(op.faction, op.isAdd, op.detail)
    stateStack.pop()

    flags.setFlagData(US, Flags.NORAD, defcon)

    tryNextRound()
  }

  def nextStateCubaMissileRemove(input: Operation): Unit = {
    val op = input.asInstanceOf[OperationSelectCountry]
    val detail = op.detail

    stateStack.pop()

    for (country <- detail) {
      modifyInfluence(input.faction, false, Map(country -> 2))
      removeFlag(input.faction, Flags.CubaMissile)
    }
  }

  def checkFlags(): Unit = {
    for ((faction, set) <- flags.flagSets) {
      for (flag <- set) {
        if (!flag.canKeep(this, faction)) {
          removeFlag(faction, flag)
        }
      }
    }
  }

  def clearSnapshots(): Unit = {
    lastSnapshot.needApproval = true
    for (h <- history) {
      if (h.snapshot != null) {
        h.snapshot.needApproval = true
      }
    }
  }

  def sendRollBackBeforeHistory(historyId: Int): Unit = {
    rollBackBeforeHistory(historyId)
    anotherGame.rollBackBeforeHistory(historyId)
  }

  override def rollBackBeforeHistory(historyId: Int): Unit = {
    history.find(_.id == historyId) match {
      case Some(history) =>
        val snapshot = history.snapshot
        if (snapshot != null && !snapshot.needApproval) {
          snapshot.rollBack()
        }
      case _ =>
    }
    currentOperationHint = createOperationHint()
    stateUpdateListeners.foreach(_())
  }

  def getOperationHint: OperationHint = currentOperationHint

  def createOperationHint(): OperationHint = {
    if (isSpectator) {
      return OperationHint.NOP
    }
    stateStack.top match {
      case State.start => OperationHint.CHOOSE_FACTION
      case State.putStartUSSR =>
        if (playerFaction == USSR) {
          OperationHint(classOf[OperationModifyInfluence], 7, true, USSR, (game, detail) =>
            detail.forall(_._1.regions(Region.EastEurope)), true, true, false)
        } else {
          OperationHint.NOP
        }
      case State.putStartUS =>
        if (playerFaction == US) {
          OperationHint(classOf[OperationModifyInfluence], 6, true, US, (game, detail) =>
            detail.forall(_._1.regions(Region.WestEurope)), true, true, false)
        } else {
          OperationHint.NOP
        }
      case State.putStartExtra =>
        if ((playerFaction == US && extraInfluence > 0) || (playerFaction == USSR && extraInfluence < 0)) {
          OperationHint(classOf[OperationModifyInfluence], Math.abs(extraInfluence), true, playerFaction,
            (game, detail) => detail.forall(e => influence(e._1, playerFaction) > 0), true, true, false)
        } else {
          OperationHint.NOP
        }
      case State.selectHeadlineCard =>
        if (!flags.hasFlag(playerFaction, Flags.SpaceAwardHeadlineThen)) {
          OperationHint(classOf[OperationSelectCard], false, (game, card) => card.canHeadline(this, playerFaction))
        } else {
          OperationHint.NOP
        }
      case State.selectHeadlineCard2 =>
        if (flags.hasFlag(playerFaction, Flags.SpaceAwardHeadlineThen)) {
          OperationHint(classOf[OperationSelectCard], false, (game, card) => card.canHeadline(this, playerFaction))
        } else {
          OperationHint.NOP
        }
      case State.selectCardAndAction =>
        if (playerFaction == operatingPlayer) {
          OperationHint(classOf[OperationSelectCardAndAction], null,
            (game, card) => card.canPlay(this, playerFaction),
            (game, card) => canCardSpace(card, playerFaction),
            (game, card) => canCardEvent(card, playerFaction),
            (game, card) => canCardOperation(card, playerFaction)
          )
        } else {
          OperationHint.NOP
        }
      case State.selectAction =>
        if (playerFaction == operatingPlayer) {
          OperationHint(classOf[OperationSelectCardAndAction], currentCard,
            (game, card) => card.canPlay(this, playerFaction),
            (game, card) => canCardSpace(card, playerFaction),
            (game, card) => canCardEvent(card, playerFaction),
            (game, card) => canCardOperation(card, playerFaction)
          )
        } else {
          OperationHint.NOP
        }
      case State.cardOperationSelect =>
        if (playerFaction == operatingPlayer) {
          OperationHint(classOf[OperationSelectOperation],
            _ => true,
            _ => true,
            _ => canCoup(playerFaction)
          )
        } else {
          OperationHint.NOP
        }
      case State.cardOperationAddInfluence =>
        if (playerFaction == operatingPlayer) {
          OperationHint(classOf[OperationModifyInfluence], currentCard.op, true, playerFaction,
            (game, detail) => canAddInfluence(playerFaction)(detail), false, true, true)
        } else {
          OperationHint.NOP
        }
      case State.cardOperationRealignment =>
        if (playerFaction == operatingPlayer) {
          OperationHint(classOf[OperationSelectCountry], 1, getCurrentRealignmentRest(playerFaction),
            (game: Game, detail: Set[Country]) => detail.forall(canRealignment(playerFaction, _)), true)
        } else {
          OperationHint.NOP
        }
      case State.cardOperationCoup =>
        if (playerFaction == operatingPlayer) {
          OperationHint(classOf[OperationSelectCountry], 1, currentCard.op,
            (game: Game, detail: Set[Country]) => detail.forall(canCoup(playerFaction, _)), true)
        } else {
          OperationHint.NOP
        }
      case State.discardHeldCard =>
        if (flags.hasFlag(playerFaction, Flags.SpaceAwardMayDiscard)) {
          OperationHint(classOf[OperationSelectCard], true, (game, card) => card.canDiscard(this, playerFaction))
        } else {
          OperationHint.NOP
        }
      case State.selectTake8Rounds =>
        if (playerFaction == phasingPlayer) {
          OperationHint(classOf[OperationYesNo], false)
        } else {
          OperationHint.NOP
        }
      case State.quagmireDiscard =>
        if (phasingPlayer == playerFaction) {
          OperationHint(classOf[OperationSelectCard], false, (game, card) =>
            card.canPlay(this, playerFaction) && card.canDiscard(this, playerFaction))
        } else {
          OperationHint.NOP
        }
      case State.quagmirePlayScoringCard =>
        if (phasingPlayer == playerFaction) {
          OperationHint(classOf[OperationSelectCard], false, (game, card) => card.canPlay(this, playerFaction))
        } else {
          OperationHint.NOP
        }
      case State.noradInfluence =>
        if (playerFaction == US) {
          OperationHint(classOf[OperationModifyInfluence], 1, true, playerFaction,
            (game, detail) => detail.forall(e => influence(e._1, Faction.US) > 0), true, true)
        } else {
          OperationHint.NOP
        }
      case State.cubaMissileRemove =>
        if (flags.hasFlag(playerFaction, Flags.CubaMissile)) {
          OperationHint(classOf[OperationSelectCountry], 1, 0,
            Card040CubaMissile.getConditionByFaction(playerFaction), false)
        } else {
          OperationHint.NOP
        }
      case EventStates(_) =>
        if (operatingPlayer == playerFaction) {
          currentCard.getOperationHint(this)
        } else {
          OperationHint.NOP
        }
      case _ => OperationHint.NOP
    }
  }

  class GameOverException(val winner: Faction) extends Exception

}
