package me.herbix.ts.logic

import me.herbix.ts.logic.Faction._
import me.herbix.ts.logic.State._

import scala.collection.mutable

/**
  * Created by Chaofan on 2016/6/12.
  */
class Game {

  var anotherGame: Game = null

  var playerId = 0
  var currentPlayer = Neutral

  val worldMap = new WorldMap

  var pendingInput: OperationInput = null
  var stateStack = mutable.Stack(start)

  var turn = 1
  var round = 0
  var current = USSR

  val military = mutable.Map(US -> 0, USSR -> 0)
  val space = mutable.Map(US -> 0, USSR -> 0)
  var vp = 0
  var defcon = 5

  val hand = Map(US -> new CardSet, USSR -> new CardSet)
  val deck = new CardSet
  val discards = new CardSet

  val flags = new Flags

  val dice = new Dice

  var stateUpdateListeners: List[() => Unit] = List()

  def sendNextState(input: OperationInput): Unit = {
    nextState(input)
    if (anotherGame != null) {
      anotherGame.nextState(input)
    }
  }

  def nextState(input: OperationInput, currentState: State): Unit = {
    currentState match {
      case State.start => nextStateStart(input)
    }
  }

  def nextState(input: OperationInput): Unit = {
    stateStack.top match {
      case State.waitOther =>
        val top = stateStack.top
        val top2 = stateStack(1)
        nextState(input, top2)
      case other => nextState(input, other)
    }
    for (listener <- stateUpdateListeners) {
      listener()
    }
  }

  def nextStateStart(input: OperationInput): Unit = {
    if (pendingInput == null) {
      pendingInput = input
      if (input.playerId == playerId) {
        stateStack.push(waitOther)
      }
    } else {
      if (stateStack.top == waitOther) {
        stateStack.pop()
      }
      val input1 = input.asInstanceOf[OperationInputChooseFaction]
      val input2 = pendingInput.asInstanceOf[OperationInputChooseFaction]

      val myInput = if (input1.playerId == playerId) input1 else input2
      val smallInput = if (input1.playerId < input2.playerId) input1 else input2

      if (input1.faction != input2.faction) {
        currentPlayer = myInput.faction
      } else {
        val smallChange = dice.nextBoolean()
        if (smallChange == (smallInput == myInput)) {
          currentPlayer = if (myInput.faction == US) USSR else US
        } else {
          currentPlayer = myInput.faction
        }
      }

      pendingInput = null

      initGame()

      stateStack.pop()
      stateStack.push(putStartUSSR)
    }
  }

  def initGame(): Unit = {
    deck.join(Cards.earlyWarSet)

    for (i <- 0 until 8) {
      hand(US).add(deck.pickAndRemove(dice))
      hand(USSR).add(deck.pickAndRemove(dice))
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
}
