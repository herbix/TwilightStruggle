package me.herbix.ts.logic

import java.util.Random

import me.herbix.ts.logic.Faction._
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
  var currentUsedOp = 0

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
      case State.selectHeadlineCard => nextStateMayWait(input, nextStateChooseHeadline)
      case State.solveHeadLineCard1 => nextStateSolveHeadline1()
      case State.solveHeadLineCard2 => nextStateSolveHeadline2()
      case State.selectCardAndAction => nextStateSelectCardAndAction(input)
      case State.cardEvent => nextStateCardEvent()
      case State.cardOperationSelect => nextStateOperationSelect(input)
      case State.cardOperationAddInfluence => nextStateOperationInfluence(input)
      case State.cardOperationRealignment => nextStateOperationRealignment(input)
      case State.cardOperationCoup => nextStateOperationCoup(input)
      case State.cardOperation => nextStateCardOperation()
    }
    if (stateStack.top == State.cardEventEnd) {
      stateStack.pop()
      nextState(null, stateStack.top)
    } else if (stateStack.top == State.cardOperation) {
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
      worldMap.countries(country.name).influence(faction) += (if (isAdd) value else -value)
      detail2 += ((country, oldInfluence, worldMap.countries(country.name).influence(faction)))
    }
    history.push(new HistoryModifyInfluence(faction, isAdd, detail2))
  }

  def discardCard(card: Card, from: Faction, force: Boolean = false): Unit = {
    if (card == Cards.chinaCard) {
      val opposite = Faction.getOpposite(from)
      hand(opposite).add(card)
      flags.addFlag(opposite, Flags.cantPlayChinaCard)
    } else if (force || !card.isRemovedAfterEvent) {
      discards.add(card)
    }
  }

  def nextStateChooseHeadline(input: Operation): Unit = {
    val input1 = input.asInstanceOf[OperationSelectCard]
    val input2 = pendingInput.asInstanceOf[OperationSelectCard]

    val inputA = if (input1.card.op > input2.card.op || (input1.card.op == input2.card.op && input1.faction == US))
      input1 else input2
    val inputB = if (input1 == inputA) input2 else input1

    hand(inputA.faction).remove(inputA.card)
    hand(inputB.faction).remove(inputB.card)

    discardCard(inputA.card, inputA.faction)
    discardCard(inputB.card, inputB.faction)

    history.push(new HistoryPlayHeadline(inputA.faction, inputA.card))
    history.push(new HistoryPlayHeadline(inputB.faction, inputB.card))

    currentCard = inputA.card
    phasingPlayer = inputA.faction

    pendingInput = inputB

    stateStack.pop()
    stateStack.push(solveHeadLineCard1)

    history.push(new HistoryTurnRound(turn, round, phasingPlayer))

    stateStack.push(cardEventStart)
    currentCard.nextState(this, null)

    history.push(new HistoryEvent(phasingPlayer, currentCard))
  }

  def nextStateSolveHeadline1() = {
    val input = pendingInput.asInstanceOf[OperationSelectCard]

    currentCard = input.card
    phasingPlayer = input.faction

    pendingInput = null

    history.push(new HistoryTurnRound(turn, round, phasingPlayer))

    stateStack.pop()
    stateStack.push(solveHeadLineCard2)

    stateStack.push(cardEventStart)
    currentCard.nextState(this, null)

    history.push(new HistoryEvent(phasingPlayer, currentCard))
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
  }

  def addVp(faction: Faction, value: Int): Unit = {
    vp += Faction.getVpFactor(faction) * value
    history.push(new HistoryVp(faction, value, vp))
  }

  def setDefcon(newVal: Int): Unit = {
    val oldVal = defcon
    defcon = newVal
    checkDefcon()
    history.push(new HistoryDefcon(oldVal, newVal))
  }

  def nextTurn(): Unit = {
    turn += 1
    if (turn > 10) {
      // TODO final scoring
      // TODO gameover
      return
    }
    round = 0

    history.push(new HistoryTurnRound(turn, round, Neutral))

    if (turn == 4) {
      deck.join(Cards.midWarSet)
    } else if (turn == 8) {
      deck.join(Cards.lateWarSet)
    }

    val usHandCount = hand(US).cardCount
    val ussrHandCount = hand(USSR).cardCount

    val handCount = turnRoundCount + 2
    for (i <- 0 until handCount) {
      if (hand(US).cardCount < handCount || (hand(US).cardCount == handCount && hand(US).has(Cards.chinaCard))) {
        hand(US).add(pickCardFromDeck())
      }
      if (hand(USSR).cardCount < handCount || (hand(USSR).cardCount == handCount && hand(USSR).has(Cards.chinaCard))) {
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

    setMilitary(US, 0)
    setMilitary(USSR, 0)
    setDefcon(defcon + 1)

    flags.turnEnds()

    stateStack.push(State.selectHeadlineCard)
  }

  def nextStateSelectCardAndAction(input: Operation): Unit = {
    val op = input.asInstanceOf[OperationSelectCardAndAction]
    val oppositeCard = op.card.faction == Faction.getOpposite(op.faction)
    val alwaysTriggerEvent = oppositeCard && op.card.canPlayAsEvent(this)

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
          stateStack.push(State.selectCardAndAction)
        } else {
          nextTurn()
        }
      case Action.Event =>
        discardCard(op.card, op.faction)
        stateStack.pop()
        if (!oppositeCard) {
          stateStack.push(State.cardE)
        } else {
          stateStack.push(State.cardEO)
        }
        stateStack.push(State.cardEvent)
        stateStack.push(State.cardEventStart)
        currentCard.nextState(this, null)
        history.push(new HistoryEvent(phasingPlayer, currentCard))
      case Action.Operation =>
        discardCard(op.card, op.faction, !alwaysTriggerEvent)
        stateStack.pop()
        if (!oppositeCard) {
          stateStack.push(State.cardO)
        } else {
          stateStack.push(State.cardOE)
        }
        stateStack.push(State.cardOperation)
        stateStack.push(State.cardOperationSelect)
    }

  }

  def canSpace(faction: Faction): Boolean = {
    space(faction) != SpaceLevel.Station &&
      (!flags.hasFlag(faction, Flags.Space1) ||
        (!flags.hasFlag(faction, Flags.Space2) && flags.hasFlag(faction, Flags.SpaceAwardTwoSpace)))
  }

  def canCardSpace(card: Card, faction: Faction): Boolean = {
    card.id != 0 && canSpace(faction) && space(faction).nextLevel.op <= card.op
  }

  def canCardEvent(card: Card, playerFaction: Faction): Boolean = {
    card.id != 0 && card.canPlayAsEvent(this)
  }

  def canCardOperation(card: Card, playerFaction: Faction): Boolean = {
    card.id != 0 && card.op > 0
  }

  def nextStateCardEvent() = {
    stateStack.pop()
    stateStack.top match {
      case State.cardEO =>
        stateStack.push(State.cardOperation)
        stateStack.push(State.cardOperationSelect)
      case State.cardOE | State.cardE =>
        currentCard = null
        stateStack.pop()
        if (nextRound()) {
          stateStack.push(State.selectCardAndAction)
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
        stateStack.push(State.cardOperationAddInfluence)
      case Action.Realignment =>
        stateStack.push(State.cardOperationRealignment)
        currentUsedOp = 0
      case Action.Coup =>
        stateStack.push(State.cardOperationCoup)
    }
  }

  def nextStateOperationInfluence(input: Operation) = {
    val op = input.asInstanceOf[OperationModifyInfluence]
    modifyInfluence(op.faction, op.isAdd, op.detail)
    stateStack.pop()
  }

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
    rollUS += worldMap.links(country.name).count(name => {
      val country = worldMap.countries(name)
      country.influence(US) - country.influence(USSR) >= country.stability
    })
    rollUSSR += worldMap.links(country.name).count(name => {
      val country = worldMap.countries(name)
      country.influence(USSR) - country.influence(US) >= country.stability
    })
    history.push(new HistoryOperationRealignment(country, rollUSOriginal, rollUSSROriginal, rollUS, rollUSSR))
    if (rollUS > rollUSSR) {
      modifyInfluence(USSR, false, Map(country -> Math.min(rollUS - rollUSSR, country.influence(USSR))))
    } else if (rollUS < rollUSSR) {
      modifyInfluence(US, false, Map(country -> Math.min(rollUSSR - rollUS, country.influence(US))))
    }
    currentUsedOp += 1
    if (currentUsedOp >= currentCard.op) {
      stateStack.pop()
    }
  }

  def checkDefcon() = {
    if (defcon <= 1) {
      defcon = 1
      // TODO gameover
    }
    if (defcon > 5) {
      defcon = 5
    }
  }

  def addMilitary(faction: Faction, op: Int) = setMilitary(faction, military(faction) + op)

  def setMilitary(faction: Faction, value: Int) = {
    val oldval = military(faction)
    military(faction) = value
    if (military(faction) > 5) {
      military(faction) = 5
    }
    if (military(faction) < 0) {
      military(faction) = 0
    }
    if (oldval != military(faction)) {
      history.push(new HistoryMilitary(faction, oldval, military(faction)))
    }
  }

  def nextStateOperationCoup(input: Operation) = {
    val op = input.asInstanceOf[OperationSelectCountry]
    val country = worldMap.countries(op.detail.head.name)
    val rollResult = rollDice()
    val coupFactor = Math.max(0, rollResult + currentCard.op - 2 * country.stability)

    history.push(new HistoryOperationCoup(op.faction, country, rollResult, currentCard.op, coupFactor))

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

    if (country.critical) {
      setDefcon(defcon - 1)
    }

    addMilitary(factionSelf, currentCard.op)

    stateStack.pop()
  }

  def nextStateCardOperation() = {
    stateStack.pop()
    stateStack.top match {
      case State.cardOE =>
        stateStack.push(State.cardEvent)
        stateStack.push(State.cardEventStart)
        currentCard.nextState(this, null)
        history.push(new HistoryEvent(phasingPlayer, currentCard))
      case State.cardEO | State.cardO =>
        currentCard = null
        stateStack.pop()
        if (nextRound()) {
          stateStack.push(State.selectCardAndAction)
        } else {
          nextTurn()
        }
    }
  }

}
