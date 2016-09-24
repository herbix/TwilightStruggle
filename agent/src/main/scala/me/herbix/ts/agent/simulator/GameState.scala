package me.herbix.ts.agent.simulator

import me.herbix.ts.logic.Faction.Faction
import me.herbix.ts.logic.{Operation, Game}
import me.herbix.ts.logic.GameVariant.GameVariant
import me.herbix.ts.logic.card.Card
import me.herbix.ts.util.Serializer

/**
  * Created by Chaofan on 2016/9/14.
  */
class GameState private(val data: Array[Byte]) extends Game {

  def this(game: Game) = {
    this(Serializer.writeGameState(game))

    playerId = game.playerId
    playerFaction = game.playerFaction
    isSpectator = game.isSpectator

    extraInfluence = game.extraInfluence
    optionalCards = game.optionalCards
    drawGameWinner = game.drawGameWinner
    gameVariant = game.gameVariant

    reset()
  }

  var nextStates = Map.empty[Operation, Map[GameState, Double]]

  override def sendNextState(input: Operation): Unit = {
    nextState(input)
  }

  override def pickCardFromDeck(): Card = {
    super.pickCardFromDeck()
  }

  override def pickCardFromHand(faction: Faction): Card = {
    super.pickCardFromHand(faction)
  }

  override def rollDice(): Int = {
    4
  }

  def reset(): Unit = {
    Serializer.readGameState(this, data)
    currentOperationHint = createOperationHint()
  }

}
