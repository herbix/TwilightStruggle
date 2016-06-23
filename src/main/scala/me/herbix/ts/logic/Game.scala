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

  def modifyInfluence(op: OperationModifyInfluence): Unit = {
    val isAdd = op.isAdd
    val faction = op.faction
    for ((country, value) <- op.detail) {
      worldMap.countries(country.name).influence(faction) += (if (isAdd) value else -value)
    }
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

    currentCard = inputA.card
    phasingPlayer = inputA.faction

    pendingInput = inputB

    stateStack.pop()
    stateStack.push(solveHeadLineCard1)

    stateStack.push(cardEventStart)
    currentCard.nextState(this, null)
  }

  def nextStateSolveHeadline1() = {
    val input = pendingInput.asInstanceOf[OperationSelectCard]

    currentCard = input.card
    phasingPlayer = input.faction

    pendingInput = null

    stateStack.pop()
    stateStack.push(solveHeadLineCard2)

    stateStack.push(cardEventStart)
    currentCard.nextState(this, null)
  }

  def nextStateSolveHeadline2() = {
    currentCard = null

    round = 1
    phasingPlayer = USSR

    stateStack.pop()
    stateStack.push(selectCardAndAction)
  }

  @tailrec
  private def nextRound(): Unit = {
    if (phasingPlayer == USSR) {
      phasingPlayer = US
    } else {
      phasingPlayer = USSR
      round += 1
    }
    if (hand(phasingPlayer).cardCount == 0) {
      nextRound()
    }
  }

  def isAfterFinalRound = if (turn <= 3) round >= 7 else round >= 8

  def rollDice() = random.nextInt(6) + 1

  def increaseSpace(faction: Faction, value: Int) = {
    var selfSpace = space(faction)
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

    val oppositeSpace = space(oppositeFaction)
    vp += Faction.getVpFactor(faction) *
      (if (oppositeSpace.level < selfSpace.level) selfSpace.firstVp else selfSpace.secondVp)
  }

  def nextStateSelectCardAndAction(input: Operation): Unit = {
    val op = input.asInstanceOf[OperationSelectCardAndAction]
    val oppositeCard = op.card.faction == Faction.getOpposite(op.faction)
    val alwaysTriggerEvent = oppositeCard && op.card.canPlayAsEvent(this)

    hand(op.faction).remove(op.card)

    if (op.action != Action.Space) {
      currentCard = op.card
    }

    op.action match {
      case Action.Space =>
        discardCard(op.card, op.faction, true)
        if (rollDice() <= space(op.faction).nextLevel.rollMax) {
          increaseSpace(op.faction, 1)
        }
        if (flags.hasFlag(op.faction, Flags.Space1)) {
          flags.addFlag(op.faction, Flags.Space2)
        } else {
          flags.addFlag(op.faction, Flags.Space1)
        }
        nextRound()
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
        nextRound()
        stateStack.pop()
        stateStack.push(State.selectCardAndAction)
    }
  }

  def nextStateOperationSelect(input: Operation) = {
    val op = input.asInstanceOf[OperationSelectOperation]
    stateStack.pop()

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
    modifyInfluence(op)
    stateStack.pop()
  }

  def nextStateOperationRealignment(input: Operation) = {
    val op = input.asInstanceOf[OperationSelectCountry]
    val country = worldMap.countries(op.detail.head.name)
    var rollUS = rollDice()
    var rollUSSR = rollDice()
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
    if (rollUS > rollUSSR) {
      country.influence(USSR) -= Math.min(rollUS - rollUSSR, country.influence(USSR))
    } else if (rollUS < rollUSSR) {
      country.influence(US) -= Math.min(rollUSSR - rollUS, country.influence(US))
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
  }

  def nextStateOperationCoup(input: Operation) = {
    val op = input.asInstanceOf[OperationSelectCountry]
    val country = worldMap.countries(op.detail.head.name)
    val coupFactor = Math.max(0, rollDice() + currentCard.op - 2 * country.stability)

    val factionSelf = op.faction
    val factionOpposite = Faction.getOpposite(factionSelf)

    val oppositeDown = Math.min(coupFactor, country.influence(factionOpposite))
    val selfUp = coupFactor - oppositeDown

    country.influence(factionOpposite) -= oppositeDown
    country.influence(factionSelf) += selfUp

    if (country.critical) {
      defcon -= 1
      checkDefcon()
    }

    stateStack.pop()
  }

  def nextStateCardOperation() = {
    stateStack.pop()
    stateStack.top match {
      case State.cardOE =>
        stateStack.push(State.cardEvent)
        stateStack.push(State.cardEventStart)
        currentCard.nextState(this, null)
      case State.cardEO | State.cardO =>
        currentCard = null
        nextRound()
        stateStack.pop()
        stateStack.push(State.selectCardAndAction)
    }
  }

}
