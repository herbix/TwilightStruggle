package me.herbix.ts.agent

import javax.security.auth.callback.Callback

import me.herbix.ts.agent.random.RandomAgent
import me.herbix.ts.logic.{OperationHint, Operation, Game}

/**
  * Created by Chaofan on 2016/9/9.
  */
object AgentFactory {

  def createDefaultAgent(game: Game, callback: (OperationHint, Operation) => Unit): Agent =
    new RandomAgent(game, callback)

  def createAgentFromClass(agentClass: Class[_ <: Agent], game: Game, callback: (OperationHint, Operation) => Unit): Agent = {
    val constructor = agentClass.getConstructor(classOf[Game], classOf[(OperationHint, Operation) => Unit])
    constructor.newInstance(game, callback)
  }

  private val allAgentClass = Set[Class[_ <: Agent]](classOf[RandomAgent])

  def getAllAgentClass: Set[Class[_ <: Agent]] = allAgentClass

}
