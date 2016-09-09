package me.herbix.ts.agent

import me.herbix.ts.logic.Game

/**
  * Created by Chaofan on 2016/9/8.
  */
class Agent(game: Game) {

  game.stateUpdateListeners :+= gameUpdateState

  gameUpdateState()

  def gameUpdateState(): Unit = {
    val oh = game.getOperationHint

  }

}
