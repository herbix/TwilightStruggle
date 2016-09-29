package me.herbix.ts.agent

import me.herbix.ts.logic._
import me.herbix.ts.util.{OperationChooseFactionHint, OperationHint}

import scala.util.Random

/**
  * Created by Chaofan on 2016/9/27.
  */
abstract class Agent(game: Game, operationCallback: (OperationHint, Operation) => Unit) extends AgentBase(game, operationCallback) {

  override def update(game: Game, hint: OperationHint): Operation = {
    val rand = new Random()

    hint match {
      case h: OperationChooseFactionHint =>
        if (game.pendingInput == null) {
          if (isOpponentAgent)
            new OperationChooseFaction(game.playerId, if (rand.nextBoolean()) Faction.US else Faction.USSR)
          else
            null
        } else {
          val pendingInput = game.pendingInput.asInstanceOf[OperationChooseFaction]
          new OperationChooseFaction(game.playerId, Faction.getOpposite(pendingInput.faction))
        }
      case _ => pickOperation(game, hint)
    }
  }

  def pickOperation(game: Game, hint: OperationHint): Operation

}
