package me.herbix.ts.agent

import me.herbix.ts.logic.{Operation, OperationHint, Game}

/**
  * Created by Chaofan on 2016/9/8.
  */
abstract class Agent(game: Game) {

  game.stateUpdateListeners :+= gameUpdateState

  gameUpdateState()

  def gameUpdateState(): Unit = {
    val input = update(game, game.getOperationHint)
    if (input != null) {
      game.sendNextState(input)
    }
  }

  abstract def update(game: Game, hint: OperationHint): Operation

}
