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
  val space = mutable.Map(US -> 0, USSR -> 0)
  var vp = 0
  var defcon = 5

  var currentEventCard: Card = null

  val hand = Map(US -> new CardSet, USSR -> new CardSet)
  val deck = new CardSet
  val discards = new CardSet

  val flags = new Flags

  var randomSeed = 0l
  val random = new Random

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
      case State.putStartUSExtra => nextStatePutStart(input, selectHeadlineCard)
      case State.selectHeadlineCard => nextStateMayWait(input, nextStateChooseHeadline)
      case State.solveHeadLineCard1 => nextStateSolveHeadline1()
      case State.solveHeadLineCard2 => nextStateSolveHeadline2()
      case State.selectCardAndAction => nextStateSelectCardAndAction(input: Operation)
    }
    if (stateStack.top == State.cardEventEnd) {
      stateStack.pop()
      nextState(null, stateStack.top)
    }
  }

  private def nextState(input: Operation): Unit = {
    stateStack.top match {
      case State.waitOther =>
        val top = stateStack.top
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

    initGame()

    stateStack.pop()
    stateStack.push(putStartUSSR)
  }

  private def initGame(): Unit = {
    deck.join(Cards.earlyWarSet)

    for (i <- 0 until 8) {
      hand(US).add(deck.pickAndRemove(random))
      hand(USSR).add(deck.pickAndRemove(random))
    }
    hand(USSR).add(Cards.chinaCard)

    worldMap.modifyInfluence("Syria", USSR, 1)
    worldMap.modifyInfluence("Iraq", USSR, 1)
    worldMap.modifyInfluence("N.Korea", USSR, 3)
    worldMap.modifyInfluence("E.Germany", USSR, 3)
    worldMap.modifyInfluence("Finland", USSR, 1)

    worldMap.modifyInfluence("Canada", US, 2)
    worldMap.modifyInfluence("Iran", US, 1)
    worldMap.modifyInfluence("Israel", US, 1)
    worldMap.modifyInfluence("Japan", US, 1)
    worldMap.modifyInfluence("Australia", US, 4)
    worldMap.modifyInfluence("Philippines", US, 1)
    worldMap.modifyInfluence("S.Korea", US, 1)
    worldMap.modifyInfluence("Panama", US, 1)
    worldMap.modifyInfluence("South Africa", US, 1)
    worldMap.modifyInfluence("UK", US, 5)
  }

  def nextStatePutStart(input: Operation, next: State): Unit = {
    val op = input.asInstanceOf[OperationModifyInfluence]
    modifyInfluence(op)
    stateStack.pop()
    stateStack.push(next)
  }

  def modifyInfluence(op: OperationModifyInfluence): Unit = {
    val isAdd = op.isAdd
    val faction = op.faction
    for ((country, value) <- op.detail) {
      worldMap.countries(country.name).influence(faction) += (if (isAdd) value else -value)
    }
  }

  def discardCard(card: Card, force: Boolean = false): Unit = {
    discards.add(card)
  }

  def nextStateChooseHeadline(input: Operation): Unit = {
    val input1 = input.asInstanceOf[OperationSelectCard]
    val input2 = pendingInput.asInstanceOf[OperationSelectCard]

    val inputA = if (input1.card.op > input2.card.op || (input1.card.op == input2.card.op && input1.faction == US))
      input1 else input2
    val inputB = if (input1 == inputA) input2 else input1

    hand(inputA.faction).remove(inputA.card)
    hand(inputB.faction).remove(inputB.card)

    discardCard(inputA.card)
    discardCard(inputB.card)

    currentEventCard = inputA.card
    phasingPlayer = inputA.faction

    pendingInput = inputB

    stateStack.pop()
    stateStack.push(solveHeadLineCard1)

    stateStack.push(cardEventStart)
    currentEventCard.nextState(this, null)
  }

  def nextStateSolveHeadline1() = {
    val input = pendingInput.asInstanceOf[OperationSelectCard]

    currentEventCard = input.card
    phasingPlayer = input.faction

    pendingInput = null

    stateStack.pop()
    stateStack.push(solveHeadLineCard2)

    stateStack.push(cardEventStart)
    currentEventCard.nextState(this, null)
  }

  def nextStateSolveHeadline2() = {
    currentEventCard = null

    round = 1
    phasingPlayer = USSR

    stateStack.pop()
    stateStack.push(selectCardAndAction)
  }

  def nextRound() = {
    if (phasingPlayer == USSR) {
      phasingPlayer = US
    } else {
      phasingPlayer = USSR
      round += 1
    }
  }

  def rollDice() = random.nextInt(6) + 1

  def increaseSpace(faction: Faction, value: Int) = {
    space(faction) += value
  }

  def nextStateSelectCardAndAction(input: Operation): Unit = {
    val op = input.asInstanceOf[OperationSelectCardAndAction]
    val alwaysTriggerEvent = op.card.faction == Faction.getOpposite(phasingPlayer) && op.card.canPlayAsEvent(this)

    hand(op.faction).remove(op.card)

    op.action match {
      case Action.Space =>
        discardCard(op.card, true)
        if (rollDice() <= 4) {
          increaseSpace(op.faction, 1)
        }
        if (flags.hasFlag(op.faction, Flags.Space1)) {
          flags.addFlag(op.faction, Flags.Space2)
        } else {
          flags.addFlag(op.faction, Flags.Space1)
        }
        nextRound()
      case Action.Event =>
        discardCard(op.card)
      case Action.Operation =>
        discardCard(op.card, !alwaysTriggerEvent)
    }

    if (op.action == Action.Event || (op.action == Action.Operation && alwaysTriggerEvent)) {
      currentEventCard = op.card
    }

  }
}
