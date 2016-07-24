package me.herbix.ts.logic.card

import me.herbix.ts.logic.Faction.Faction
import me.herbix.ts.logic.Faction._
import me.herbix.ts.logic._

/**
  * Created by Chaofan on 2016/7/23.
  */
abstract class Card(val id: Int, val op: Int, val faction: Faction, val isRemovedAfterEvent: Boolean) extends Ordered[Card] {
  def canHeadline(game: Game, faction: Faction) = canEvent(game, faction)
  def canDiscard(game: Game, faction: Faction): Boolean = {
    if (game.flags.hasFlag(faction, Flags.QuagmireBearTrap)) {
      return canHeld(game)
    }
    true
  }
  def canEvent(game: Game, faction: Faction) = true
  def canPlay(game: Game, faction: Faction): Boolean = {
    if (game.flags.hasFlag(faction, Flags.QuagmireBearTrap)) {
      return !canHeld(game) ||
        ((game.modifyOp(faction, op) >= 2 && canDiscard(game, faction)) &&
          !(game.flags.hasFlag(faction, Flags.MissileEnvy) && this != Card049MissileEnvy))
    }
    if (game.flags.hasFlag(faction, Flags.MissileEnvy)) {
      val scoringCardCount = game.hand(faction).count(!_.canHeld(game))
      return (scoringCardCount >= game.turnRoundCount + 1 - game.round && !canHeld(game)) || this == Card049MissileEnvy
    }
    true
  }
  def canHeld(game: Game) = true
  def afterPlay(game: Game, faction: Faction): Unit = {}
  def modifyOp(game: Game, faction: Faction, originalOp: Int, targets: Iterable[Country]): Int = originalOp
  def modifyOp(game: Game, faction: Faction, originalOp: Int): Int = originalOp
  def getOperatingPlayer(operatingPlayer: Faction): Faction = if (faction == Neutral) operatingPlayer else faction

  def getOperationHint(implicit game: Game): OperationHint = OperationHint.NOP

  override def toString: String = f"Card($id)"
  override def compare(that: Card): Int = {
    if (that.op < op) 1 else if (that.op > op) -1 else
    if (that == Cards.chinaCard) -1 else if (this == Cards.chinaCard) 1 else
    if (that.id < id) 1 else if (that.id > id) -1 else
    if (that.## < ##) 1 else if (that.## > ##) -1 else 0
  }

  /**
    * transfer to next state
    * @param game game instance
    * @param faction US or USSR
    * @param input the operation
    */
  def nextState(game: Game, faction: Faction, input: Operation): Unit

  // util methods
  def realignment(game: Game, input: Operation): Boolean = {
    var rest = game.currentCardData.asInstanceOf[Int] - 1
    val detail = input.asInstanceOf[OperationSelectCountry].detail
    if (detail.nonEmpty) {
      game.realignment(detail.head)
    } else {
      rest = 0
    }
    if (rest == 0) {
      game.currentCardData = null
      true
    } else {
      game.currentCardData = rest
      false
    }
  }
}
