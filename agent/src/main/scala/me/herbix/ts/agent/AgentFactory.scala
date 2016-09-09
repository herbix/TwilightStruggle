package me.herbix.ts.agent

import javax.security.auth.callback.Callback

import me.herbix.ts.agent.random.RandomAgent
import me.herbix.ts.logic.{Operation, Game}

/**
  * Created by Chaofan on 2016/9/9.
  */
object AgentFactory {

  def createDefaultAgent(game: Game, callback: Operation => Unit): Agent =
    new RandomAgent(game, callback)

}
