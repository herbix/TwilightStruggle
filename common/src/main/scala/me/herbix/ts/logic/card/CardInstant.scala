// Copyright (C) 2017 Chaofan

package me.herbix.ts.logic.card

import me.herbix.ts.logic.Faction._
import me.herbix.ts.logic.State._
import me.herbix.ts.logic.{Operation, Game}

/**
  * Created by Chaofan on 2016/7/23.
  */

abstract class CardInstant(id: Int, op: Int, faction: Faction, isRemovedAfterEvent: Boolean)
  extends Card(id, op, faction, isRemovedAfterEvent) {

  /**
    * do instant event
 *
    * @param game game instance
    * @param faction US or USSR
    * @return true if event ends normally
    *         false if handle state stack by card
    */
  def instantEvent(game: Game, faction: Faction): Boolean

  def nextState(game: Game, faction: Faction, input: Operation): Unit = {
    if (game.stateStack.top == cardEventStart) {
      if (instantEvent(game, faction)) {
        game.stateStack.pop()
        game.stateStack.push(cardEventEnd)
      }
    }
  }
}
