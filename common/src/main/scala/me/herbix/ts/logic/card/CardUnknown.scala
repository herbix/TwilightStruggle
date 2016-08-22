package me.herbix.ts.logic.card

import me.herbix.ts.logic.Faction._
import me.herbix.ts.logic.{Operation, Game}

/**
  * Created by Chaofan on 2016/7/23.
  */
object CardUnknown extends Card(0, 0, Neutral, false) {
  override def nextState(game: Game, faction: Faction, input: Operation): Unit = throw new NotImplementedError()
}
