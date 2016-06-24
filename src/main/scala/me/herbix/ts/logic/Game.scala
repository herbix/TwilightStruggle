package me.herbix.ts.logic

import java.util.Random

import me.herbix.ts.logic.Faction._
import me.herbix.ts.logic.Region.Region
import me.herbix.ts.logic.State._

import scala.annotation.tailrec
import scala.collection.mutable

/**
  * Created by Chaofan on 2016/6/12.
  */
class Game {

  var anotherGame: Game = null

  var playerId = 0
  var playerFaction = Neutral

  val worldMap = new WorldMap

  var pendingInput: Operation = null
  var stateStack = mutable.Stack(start)

  var turn = 1
  var round = 0
  var phasingPlayer = USSR

  val military = mutable.Map(US -> 0, USSR -> 0)
  val space = mutable.Map(US -> SpaceLevel(0), USSR -> SpaceLevel(0))
  var vp = 0
  var defcon = 5

  var currentCard: Card = null
  var currentRealignments = List[Country]()

  val hand = Map(US -> new CardSet, USSR -> new CardSet)
  val deck = new CardSet
  val discards = new CardSet

  val flags = new Flags

  var randomSeed = 0l
  val random = new Random

  val history = mutable.Stack[History]()

  var stateUpdateListeners: List[() => Unit] = List()

  def sendNextState(input: Operation): Unit = {
    nextState(input)
    if (anotherGame != null) {
      anotherGame.nextState(input)
    }
  }

  @tailrec
  private def nextState(input: Operation, currentState: State): Unit = {
    currentState match {
      case State.start => nextStateMayWait(input, nextStateStart)
      case State.putStartUSSR => nextStatePutStart(input, putStartUS)
      case State.putStartUS => nextStatePutStart(input, putStartUSExtra)
      case State.putStartUSExtra => nextStatePutUSExtra(input)
      case State.selectHeadlineCard =>
        if (!flags.hasFlag(Flags.SpaceAwardHeadlineThen))
          nextStateMayWait(input, nextStateSelectHeadline)
        else
          nextStateSelectHeadlineFirst(input)
      case State.selectHeadlineCard2 => nextStateSelectHeadline(input)
      case State.solveHeadLineCard1 => nextStateSolveHeadline1()
      case State.solveHeadLineCard2 => nextStateSolveHeadline2()
      case State.selectCardAndAction => nextStateSelectCardAndAction(input)
      case State.cardEvent => nextStateCardEvent()
      case State.cardOperationSelect => nextStateOperationSelect(input)
      case State.cardOperationAddInfluence => nextStateOperationInfluence(input)
      case State.cardOperationRealignment => nextStateOperationRealignment(input)
      case State.cardOperationCoup => nextStateOperationCoup(input)
      case State.cardOperation => nextStateCardOperation()
      case State.discardHeldCard => nextStateDiscardHeldCard(input)
      case State.selectTake8Rounds => nextStateSelectTake8Rounds(input)
    }
    if (stateStack.top == cardEventEnd) {
      stateStack.pop()
      nextState(null, stateStack.top)
    } else if (stateStack.top == cardOperation) {
      nextState(null, stateStack.top)
    }
  }

  private def nextState(input: Operation): Unit = {
    stateStack.top match {
      case State.waitOther =>
        val top2 = stateStack(1)
        nextState(input, top2)
      case other => nextState(input, other)
    }
    stateUpdateListeners.foreach(_())
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

    history.push(new HistoryStartGame)
    history.push(new HistoryTurnRound(turn, -1, Neutral))

    initGame()

    stateStack.pop()
    stateStack.push(putStartUSSR)
  }

  def nextStatePutUSExtra(input: Operation): history.type = {
    nextStatePutStart(input, selectHeadlineCard)
    history.push(new HistoryTurnRound(turn, round, Neutral))
  }

  private def pickCardFromDeck(): Card = {
    if (deck.cardCount == 0) {
      deck.join(discards)
      discards.clear()
    }
    deck.pickAndRemove(random)
  }

  private def initGame(): Unit = {
    deck.join(Cards.earlyWarSet)

    for (i <- 0 until 8) {
      hand(US).add(pickCardFromDeck())
      hand(USSR).add(pickCardFromDeck())
    }
    history.push(new HistoryPickCard(US, 8))
    history.push(new HistoryPickCard(USSR, 8))

    hand(USSR).add(Cards.chinaCard)
    history.push(new HistoryGetCard(USSR, Cards.chinaCard))

    modifyInfluence(USSR, true, Map(
      worldMap.countries("Syria") -> 1,
      worldMap.countries("Iraq") -> 1,
      worldMap.countries("N.Korea") -> 3,
      worldMap.countries("E.Germany") -> 3,
      worldMap.countries("Finland") -> 1
    ))

    modifyInfluence(US, true, Map(
      worldMap.countries("Canada") -> 2,
      worldMap.countries("Iran") -> 1,
      worldMap.countries("Israel") -> 1,
      worldMap.countries("Japan") -> 1,
      worldMap.countries("Australia") -> 4,
      worldMap.countries("Philippines") -> 1,
      worldMap.countries("S.Korea") -> 1,
      worldMap.countries("Panama") -> 1,
      worldMap.countries("South Africa") -> 1,
      worldMap.countries("UK") -> 5
    ))
  }

  def nextStatePutStart(input: Operation, next: State): Unit = {
    val op = input.asInstanceOf[OperationModifyInfluence]
    modifyInfluence(op.faction, op.isAdd, op.detail)
    stateStack.pop()
    stateStack.push(next)
  }

  def calculateInfluenceCost(pendingInfluenceChange: mutable.Map[Country, Int], faction: Faction, ignoreControl: Boolean): Int = {
    var cost = 0
    for ((country, modifyValue) <- pendingInfluenceChange) {
      val influence = country.influence(faction)
      val c =
        if (ignoreControl)
          modifyValue
        else {
          val influenceOpposite = country.influence(Faction.getOpposite(faction))
          if (influenceOpposite - influence >= country.stability) {
            modifyValue + Math.min(modifyValue, influenceOpposite - influence - country.stability + 1)
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
      val oldInfluence = worldMap.countries(country.name).influence(faction)
      val newInfluence = Math.max(0, oldInfluence + (if (isAdd) value else -value))
      worldMap.countries(country.name).influence(faction) = newInfluence
      if (oldInfluence != newInfluence) {
        detail2 += ((country, oldInfluence, newInfluence))
      }
    }
    history.push(new HistoryModifyInfluence(faction, isAdd, detail2))
  }

  def discardCard(card: Card, from: Faction, force: Boolean = false, showInHistory: Boolean = false): Unit = {
    if (showInHistory) {
      history.push(new HistoryDiscardCard(from, card))
    }
    if (card == Cards.chinaCard) {
      val opposite = Faction.getOpposite(from)
      hand(opposite).add(card)
      history.push(new HistoryGetCard(opposite, card))
      flags.addFlag(opposite, Flags.cantPlayChinaCard)
    } else if (force || !card.isRemovedAfterEvent) {
      discards.add(card)
    }
  }

  def nextStateSelectHeadlineFirst(input: Operation): Unit = {
    val op = input.asInstanceOf[OperationSelectCard]
    pendingInput = input
    history.push(new HistoryPlayHeadline(op.faction, op.card))
    stateStack.pop()
    stateStack.push(selectHeadlineCard2)
  }

  def nextStateSelectHeadline(input: Operation): Unit = {
    val input1 = input.asInstanceOf[OperationSelectCard]
    val input2 = pendingInput.asInstanceOf[OperationSelectCard]

    val inputA = if (input1.card.op > input2.card.op || (input1.card.op == input2.card.op && input1.faction == US))
      input1 else input2
    val inputB = if (input1 == inputA) input2 else input1

    hand(inputA.faction).remove(inputA.card)
    hand(inputB.faction).remove(inputB.card)

    discardCard(inputA.card, inputA.faction)
    discardCard(inputB.card, inputB.faction)

    if (stateStack.top == selectHeadlineCard2) {
      history.push(new HistoryPlayHeadline(input1.faction, input1.card))
    } else {
      history.push(new HistoryPlayHeadline(inputA.faction, inputA.card))
      history.push(new HistoryPlayHeadline(inputB.faction, inputB.card))
    }

    currentCard = inputA.card
    phasingPlayer = inputA.faction

    pendingInput = inputB

    stateStack.pop()
    stateStack.push(solveHeadLineCard1)

    history.push(new HistoryTurnRound(turn, round, phasingPlayer))

    history.push(new HistoryEvent(phasingPlayer, currentCard))

    stateStack.push(cardEventStart)
    currentCard.nextState(this, null)
  }

  def nextStateSolveHeadline1() = {
    val input = pendingInput.asInstanceOf[OperationSelectCard]

    currentCard = input.card
    phasingPlayer = input.faction

    pendingInput = null

    history.push(new HistoryTurnRound(turn, round, phasingPlayer))

    stateStack.pop()
    stateStack.push(solveHeadLineCard2)

    history.push(new HistoryEvent(phasingPlayer, currentCard))

    stateStack.push(cardEventStart)
    currentCard.nextState(this, null)

  }

  def nextStateSolveHeadline2() = {
    currentCard = null

    round = 1
    phasingPlayer = USSR

    stateStack.pop()
    stateStack.push(selectCardAndAction)

    history.push(new HistoryTurnRound(turn, round, phasingPlayer))
  }

  @tailrec
  private def nextRound(): Boolean = {
    if (phasingPlayer == USSR) {
      phasingPlayer = US
    } else {
      phasingPlayer = USSR
      round += 1
    }
    if (hand(phasingPlayer).cardCount == 0 && !isAfterFinalRound) {
      nextRound()
    } else {
      if (!isAfterFinalRound) {
        history.push(new HistoryTurnRound(turn, round, phasingPlayer))
        true
      } else {
        false
      }
    }
  }

  def isAfterFinalRound = round > turnRoundCount
  def turnRoundCount = if (turn <= 3) 6 else 7

  def rollDice() = random.nextInt(6) + 1

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
            flags.removeFlag(oppositeFaction, selfSpace.flag)
          } else {
            flags.addFlag(faction, selfSpace.flag)
          }
        }
      }
    }
    space(faction) = selfSpace

    history.push(new HistorySpace(faction, oldSpace.level, selfSpace.level))

    val oppositeSpace = space(oppositeFaction)
    addVp(faction, if (oppositeSpace.level < selfSpace.level) selfSpace.firstVp else selfSpace.secondVp)
    checkVp()
  }

  def addVp(faction: Faction, value: Int): Unit = {
    vp += Faction.getVpFactor(faction) * value
    if (value != 0) {
      history.push(new HistoryVp(faction, value, vp))
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

  def setDefcon(newVal: Int): Unit = {
    val oldVal = defcon
    defcon = newVal
    checkDefcon()
    history.push(new HistoryDefcon(oldVal, defcon))
  }

  def nextTurn(dont8Rounds: Boolean = false, dontDiscardHeld: Boolean = false): Unit = {
    if (flags.hasFlag(Flags.SpaceAwardTake8Rounds) && round == turnRoundCount + 1 && !dont8Rounds) {
      stateStack.push(selectTake8Rounds)
      return
    }

    val usFail = hand(US).exists(!_.canHeld(this))
    val ussrFail = hand(USSR).exists(!_.canHeld(this))

    if (usFail != ussrFail) {
      if (usFail) gameOver(USSR) else gameOver(US)
    } else {
      gameOver(US)
    }

    if (flags.hasFlag(Flags.SpaceAwardMayDiscard) && !dontDiscardHeld) {
      val faction = if (flags.hasFlag(US, Flags.SpaceAwardMayDiscard)) US else USSR
      if (hand(faction).exists(_.canDiscard(this))) {
        stateStack.push(discardHeldCard)
        return
      }
    }

    turn += 1
    if (turn > 10) {
      finalScoring()
      if (vp < 0) {
        gameOver(USSR)
      } else {
        gameOver(US)
      }
      return
    }
    round = 0

    history.push(new HistoryTurnRound(turn, -1, Neutral))

    if (turn == 4) {
      deck.join(Cards.midWarSet)
    } else if (turn == 8) {
      deck.join(Cards.lateWarSet)
    }

    val usHandCount = hand(US).cardCount
    val ussrHandCount = hand(USSR).cardCount

    val handCount = turnRoundCount + 2
    for (i <- 0 until handCount) {
      if (hand(US).cardCountExcludingChinaCard < handCount) {
        hand(US).add(pickCardFromDeck())
      }
      if (hand(USSR).cardCountExcludingChinaCard < handCount) {
        hand(USSR).add(pickCardFromDeck())
      }
    }

    history.push(new HistoryPickCard(US, hand(US).cardCount - usHandCount))
    history.push(new HistoryPickCard(USSR, hand(USSR).cardCount - ussrHandCount))

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

    history.push(new HistoryTurnRound(turn, round, Neutral))

    stateStack.push(selectHeadlineCard)
  }

  def nextStateSelectCardAndAction(input: Operation): Unit = {
    val op = input.asInstanceOf[OperationSelectCardAndAction]
    val oppositeCard = op.card.faction == Faction.getOpposite(op.faction)
    val alwaysTriggerEvent = oppositeCard && op.card.canEvent(this, op.faction)

    hand(op.faction).remove(op.card)

    if (op.action != Action.Space) {
      currentCard = op.card
    }

    history.push(new HistoryCardAction(op.faction, op.card, op.action, oppositeCard))

    op.action match {
      case Action.Space =>
        discardCard(op.card, op.faction, true)
        val roll = rollDice()
        history.push(new HistoryOperationSpace(op.faction, roll))
        if (roll <= space(op.faction).nextLevel.rollMax) {
          increaseSpace(op.faction, 1)
        }
        if (flags.hasFlag(op.faction, Flags.Space1)) {
          flags.addFlag(op.faction, Flags.Space2)
        } else {
          flags.addFlag(op.faction, Flags.Space1)
        }
        stateStack.pop()
        if (nextRound()) {
          stateStack.push(selectCardAndAction)
        } else {
          nextTurn()
        }
      case Action.Event =>
        discardCard(op.card, op.faction)
        stateStack.pop()
        if (!oppositeCard) {
          stateStack.push(cardE)
        } else {
          stateStack.push(cardEO)
        }
        history.push(new HistoryEvent(phasingPlayer, currentCard))
        stateStack.push(cardEvent)
        stateStack.push(cardEventStart)
        currentCard.nextState(this, null)
      case Action.Operation =>
        discardCard(op.card, op.faction, !alwaysTriggerEvent)
        stateStack.pop()
        if (!oppositeCard) {
          stateStack.push(cardO)
        } else {
          stateStack.push(cardOE)
        }
        stateStack.push(cardOperation)
        stateStack.push(cardOperationSelect)
    }

  }

  def canSpace(faction: Faction): Boolean = {
    space(faction) != SpaceLevel.Station &&
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
    stateStack.pop()
    stateStack.top match {
      case State.cardEO =>
        stateStack.push(cardOperation)
        stateStack.push(cardOperationSelect)
      case State.cardOE | State.cardE =>
        currentCard = null
        stateStack.pop()
        if (nextRound()) {
          stateStack.push(selectCardAndAction)
        } else {
          nextTurn()
        }
    }
  }

  def nextStateOperationSelect(input: Operation) = {
    val op = input.asInstanceOf[OperationSelectOperation]
    stateStack.pop()

    history.push(new HistoryCardOperation(op.faction, currentCard, op.action))

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

  def nextStateOperationInfluence(input: Operation) = {
    val op = input.asInstanceOf[OperationModifyInfluence]
    modifyInfluence(op.faction, op.isAdd, op.detail)
    stateStack.pop()
  }

  def canRealignment(faction: Faction, country: Country): Boolean = {
    if (country.regions.contains(Region.Super)) return false
    if (modifyOp(faction, currentCard.op, currentRealignments :+ country) - currentRealignments.size - 1 < 0) {
      return false
    }
    for (Some(can) <- flags.flagSets2(faction).toStream.map(_.canRealignment(country)).find(_.isDefined)) {
      return can
    }
    true
  }

  def canCoup(faction: Faction, country: Country): Boolean = {
    if (country.regions.contains(Region.Super)) return false
    if (country.influence(Faction.getOpposite(faction)) <= 0) {
      return false
    }
    for (Some(can) <- flags.flagSets2(faction).toStream.map(_.canRealignment(country)).find(_.isDefined)) {
      return can
    }
    true
  }

  def canCoup(faction: Faction): Boolean = {
    worldMap.countries.exists(e => canCoup(faction, e._2))
  }

  def getCurrentRealignmentRest(faction: Faction): Int =
    modifyOp(faction, currentCard.op, currentRealignments) - currentRealignments.size

  def nextStateOperationRealignment(input: Operation) = {
    val op = input.asInstanceOf[OperationSelectCountry]
    val country = worldMap.countries(op.detail.head.name)
    val rollUSOriginal = rollDice()
    val rollUSSROriginal = rollDice()
    var rollUS = rollUSOriginal
    var rollUSSR = rollUSSROriginal
    if (country.influence(US) > country.influence(USSR)) {
      rollUS += 1
    } else if (country.influence(US) < country.influence(USSR)) {
      rollUSSR += 1
    }
    rollUS += worldMap.links(country.name).count(worldMap.countries(_).getController == US)
    rollUSSR += worldMap.links(country.name).count(worldMap.countries(_).getController == USSR)
    history.push(new HistoryOperationRealignment(country, rollUSOriginal, rollUSSROriginal, rollUS, rollUSSR))
    if (rollUS > rollUSSR) {
      val decreaseValue = Math.min(rollUS - rollUSSR, country.influence(USSR))
      if (decreaseValue > 0) modifyInfluence(USSR, false, Map(country -> decreaseValue))
    } else if (rollUS < rollUSSR) {
      val decreaseValue = Math.min(rollUSSR - rollUS, country.influence(US))
      if (decreaseValue > 0) modifyInfluence(US, false, Map(country -> decreaseValue))
    }
    currentRealignments = currentRealignments :+ country
    if (getCurrentRealignmentRest(op.faction) <= 0) {
      stateStack.pop()
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
    if (defcon <= 4) flags.addFlag(Neutral, Flags.Defcon4Penalty) else flags.removeFlag(Neutral, Flags.Defcon4Penalty)
    if (defcon <= 3) flags.addFlag(Neutral, Flags.Defcon3Penalty) else flags.removeFlag(Neutral, Flags.Defcon3Penalty)
    if (defcon <= 2) flags.addFlag(Neutral, Flags.Defcon2Penalty) else flags.removeFlag(Neutral, Flags.Defcon2Penalty)
  }

  def addMilitary(faction: Faction, op: Int) = setMilitary(faction, military(faction) + op)

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
      history.push(new HistoryMilitary(faction, oldValue, military(faction)))
    }
  }

  def nextStateOperationCoup(input: Operation) = {
    val op = input.asInstanceOf[OperationSelectCountry]
    val country = worldMap.countries(op.detail.head.name)
    val rollResult = rollDice()
    val modifiedOp = modifyOp(op.faction, currentCard.op, List(country))
    val coupFactor = Math.max(0, rollResult + modifiedOp - 2 * country.stability)

    history.push(new HistoryOperationCoup(op.faction, country, rollResult, modifiedOp, coupFactor))

    val factionSelf = op.faction
    val factionOpposite = Faction.getOpposite(factionSelf)

    val oppositeDown = Math.min(coupFactor, country.influence(factionOpposite))
    val selfUp = coupFactor - oppositeDown

    if (oppositeDown > 0) {
      modifyInfluence(factionOpposite, false, Map(country -> oppositeDown))
    }
    if (selfUp > 0) {
      modifyInfluence(factionSelf, true, Map(country -> selfUp))
    }

    if (country.isBattlefield) {
      setDefcon(defcon - 1)
    }

    addMilitary(factionSelf, modifiedOp)

    stateStack.pop()
  }

  def nextStateCardOperation() = {
    stateStack.pop()
    stateStack.top match {
      case State.cardOE =>
        history.push(new HistoryEvent(phasingPlayer, currentCard))
        stateStack.push(cardEvent)
        stateStack.push(cardEventStart)
        currentCard.nextState(this, null)
      case State.cardEO | State.cardO =>
        currentCard = null
        stateStack.pop()
        if (nextRound()) {
          stateStack.push(selectCardAndAction)
        } else {
          nextTurn()
        }
    }
  }

  def nextStateDiscardHeldCard(input: Operation) = {
    val op = input.asInstanceOf[OperationSelectCard]

    if (op.card != null) {
      hand(op.faction).remove(op.card)
      discardCard(op.card, op.faction, true, true)
    }

    stateStack.pop()
    nextTurn(true, true)
  }

  def nextStateSelectTake8Rounds(input: Operation) = {
    val op = input.asInstanceOf[OperationYesNo]

    stateStack.pop()
    if (op.value) {
      phasingPlayer = op.faction
      stateStack.push(selectCardAndAction)
      history.push(new HistoryTurnRound(turn, round, phasingPlayer))
    } else {
      nextTurn(true)
    }
  }

  def modifyOp(faction: Faction, originalOp: Int, targets: Iterable[Country]): Int = {
    var modifiedOp = modifyOp(faction, originalOp)
    if (currentCard != null) {
      modifiedOp = currentCard.modifyOp(faction, originalOp, targets)
    }
    if (flags.hasFlag(faction, Flags.VietnamRevolts) && targets.forall(_.regions.contains(Region.Asia))) {
      modifiedOp += 1
    }
    modifiedOp
  }

  def modifyOp(faction: Faction, originalOp: Int): Int = {
    var modifiedOp = originalOp
    if (currentCard != null) {
      if (flags.hasFlag(faction, Flags.Containment)) {
        modifiedOp += 1
      }
      if (modifiedOp > 4) modifiedOp = 4
      if (modifiedOp < 1) modifiedOp = 1
      modifiedOp = currentCard.modifyOp(faction, originalOp)
    }
    modifiedOp
  }

  def war(faction: Faction, country: Country, modifier: Int, minRoll: Int, military: Int, vp: Int) = {
    val dice = rollDice()
    val modified = dice - minRoll
    addMilitary(faction, military)
    history.push(new HistoryWar(faction, country, dice, modified))
    if (modified >= minRoll) {
      addVp(faction, vp)
      val influence = country.influence(Faction.getOpposite(faction))
      modifyInfluence(Faction.getOpposite(faction), false, Map(country -> influence))
      modifyInfluence(faction, true, Map(country -> influence))
      checkVp()
    }
  }

  def scoring(region: Region, presence: Int, domination: Int, control: Int): Unit = {
    val targetCountries = worldMap.countries.values.filter(_.regions.contains(region))
    val battlefieldCount = targetCountries.count(_.isBattlefield)
    val usBattlefield = targetCountries.count(country => country.isBattlefield && country.getController == US)
    val usNonBattlefield = targetCountries.count(country => !country.isBattlefield && country.getController == US)
    val ussrBattlefield = targetCountries.count(country => country.isBattlefield && country.getController == USSR)
    val ussrNonBattlefield = targetCountries.count(country => !country.isBattlefield && country.getController == USSR)
    val usAll = usBattlefield + usNonBattlefield
    val ussrAll = ussrBattlefield + ussrNonBattlefield

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

    usVp += targetCountries.count(country => country.getController == US && worldMap.links(country.name).contains("USSR"))
    ussrVp += targetCountries.count(country => country.getController == USSR && worldMap.links(country.name).contains("US"))

    history.push(new HistoryScoring(region, usBattlefield, ussrBattlefield, usAll, ussrAll))

    addVp(US, usVp)
    addVp(USSR, ussrVp)
  }

  def finalScoring(): Unit = {

  }

  def gameOver(faction: Faction): Unit = {
    // TODO
  }

}
