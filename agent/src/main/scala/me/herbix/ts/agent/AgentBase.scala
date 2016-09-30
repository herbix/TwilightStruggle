package me.herbix.ts.agent

import me.herbix.ts.logic.{State, Operation, Game}
import me.herbix.ts.util.OperationHint

import scala.collection.mutable

/**
  * Created by Chaofan on 2016/9/8.
  */
abstract class AgentBase(game: Game, operationCallback: (OperationHint, Operation) => Unit) {

  game.stateUpdateListeners :+= (() => {
    gameUpdateState()
  })

  var isOpponentAgent = false

  val tasks = mutable.Queue.empty[() => Unit]

  val agentThread = new Thread() {
    override def run(): Unit = agentLoop()
  }

  gameUpdateState()

  private def gameUpdateState(): Unit = {
    val hint = game.getOperationHint
    updateInstant(hint)
    tasks.enqueue(gameUpdateState(hint))
    tasks.synchronized {
      tasks.notify()
    }
  }

  private def gameUpdateState(hint: OperationHint)(): Unit = {
    if (hint == OperationHint.NOP) {
      return
    }
    val input = update(hint)
    if (input != null) {
      println(s"$this call operationCallback $input")
      operationCallback(hint, input)
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

  def updateInstant(hint: OperationHint): Unit = {}

  def update(hint: OperationHint): Operation

  def start(): Unit = {
    agentThread.start()
  }

}
