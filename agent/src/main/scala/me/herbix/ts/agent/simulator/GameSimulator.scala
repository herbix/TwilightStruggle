package me.herbix.ts.agent.simulator

import me.herbix.ts.logic.{Game, Operation}

import scala.collection.mutable

/**
  * Created by Chaofan on 2016/9/10.
  */
class GameSimulator {

  var rootGameState: GameState = null
  var stateStack = mutable.Stack[GameState]()

  def begin(game: Game): Unit = {
    rootGameState = new GameState(game)
    stateStack.clear()
    stateStack.push(rootGameState)
  }

  def extendState(input: Operation): Map[GameState, Double] = {
    val gameState = stateStack.top

    if (gameState.nextStates.contains(input)) {
      return gameState.nextStates(input)
    }

    gameState.nextState(input)

    val nextGameState = new GameState(gameState)
    val result = Map[GameState, Double](nextGameState -> 1)

    gameState.nextStates += (input -> result)

    gameState.reset()

    stateStack.push(nextGameState)

    result
  }

  def state = stateStack.top

  def rollback(): Unit = stateStack.pop()

}
