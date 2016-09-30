package me.herbix.ts.agent

import javax.security.auth.callback.Callback

import me.herbix.ts.agent.random.RandomAgent
import me.herbix.ts.logic.{Operation, Game}
import me.herbix.ts.agent.simple.SimpleAgent
import me.herbix.ts.util.OperationHint

/**
  * Created by Chaofan on 2016/9/9.
  */
object AgentFactory {

  def createDefaultAgent(game: Game, callback: (OperationHint, Operation) => Unit, isOpponentAgent: Boolean = false): Agent = {
    val result = new RandomAgent(game, callback)
    result.isOpponentAgent = isOpponentAgent
    result.start()
    result
  }

  def createAgentFromClass(agentClass: Class[_ <: Agent], game: Game, callback: (OperationHint, Operation) => Unit, isOpponentAgent: Boolean = false): Agent = {
    val constructor = agentClass.getConstructor(classOf[Game], classOf[(OperationHint, Operation) => Unit])
    val result = constructor.newInstance(game, callback)
    result.isOpponentAgent = isOpponentAgent
    result.start()
    result
  }

  private val allAgentClass = Set[Class[_ <: Agent]](
    classOf[RandomAgent],
    classOf[SimpleAgent]
  )

  def getAllAgentClass: Set[Class[_ <: Agent]] = allAgentClass

}
