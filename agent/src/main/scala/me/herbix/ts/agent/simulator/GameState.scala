package me.herbix.ts.agent.simulator

import me.herbix.ts.logic.Faction.Faction
import me.herbix.ts.logic.Game
import me.herbix.ts.logic.GameVariant.GameVariant
import me.herbix.ts.logic.card.Card

/**
  * Created by Chaofan on 2016/9/14.
  */
class GameState(variant: GameVariant) extends Game(variant) {

  override def pickCardFromDeck(): Card = {
    super.pickCardFromDeck()
  }

  override def pickCardFromHand(faction: Faction): Card = {
    super.pickCardFromHand(faction)
  }

  override def rollDice(): Int = {
    super.rollDice()
  }

}
