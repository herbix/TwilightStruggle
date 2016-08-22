package me.herbix.ts.logic.card

import me.herbix.ts.logic.Faction._
import me.herbix.ts.logic.Game
import me.herbix.ts.logic.Region._

/**
  * Created by Chaofan on 2016/7/23.
  */

class CardScoring(id: Int, val region: Region)
  extends CardInstant(id, 0, Neutral, false) {
  override def canHeld(game: Game) = false
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    game.scoring(region)
    game.checkVp()
    true
  }
}
