package me.herbix.ts.agent

import me.herbix.ts.logic.{State, Operation, OperationHint, Game}

import scala.collection.mutable

/**
  * Created by Chaofan on 2016/9/8.
  */
abstract class Agent(game: Game, operationCallback: (Operation) => Unit) {

  game.stateUpdateListeners :+= gameUpdateState

  gameUpdateState()

  val tasks = mutable.Queue.empty[() => Unit]

  val agentThread = new Thread() {
    override def run(): Unit = agentLoop()
  }

  agentThread.start()

  def gameUpdateState(): Unit = {
    tasks.enqueue(gameUpdateState(game, game.getOperationHint))
  }

  private def gameUpdateState(game: Game, hint: OperationHint)(): Unit = {
    val input = update(game, game.getOperationHint)
    if (input != null) {
      operationCallback(input)
    }
  }

  private def agentLoop(): Unit = {
    while(game.stateStack.isEmpty || game.stateStack.top != State.end) {
      while (tasks.nonEmpty) {
        tasks.dequeue().apply()
      }

      tasks.synchronized {
        tasks.wait()
      }
    }
  }

  abstract def update(game: Game, hint: OperationHint): Operation

}
