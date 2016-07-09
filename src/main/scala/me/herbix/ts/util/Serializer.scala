package me.herbix.ts.util

import java.io.DataInputStream

import me.herbix.ts.logic.Action.Action
import me.herbix.ts.logic.Faction.Faction
import me.herbix.ts.logic.Region.Region
import me.herbix.ts.logic._

import scala.collection.mutable

/**
  * Created by Chaofan on 2016/7/5.
  */
object Serializer {

  def readOperation(in: DataInputStream, game: Game): Operation = {
    val playerId = in.readInt()
    val faction = Faction(in.readInt())
    val opId = in.readByte()

    opId match {
      case 0 =>
        new OperationChooseFaction(playerId, faction)
      case 1 =>
        val isAdd = in.readBoolean()
        val len = in.readInt()
        val detail = mutable.Map.empty[Country, Int]
        for (i <- 0 until len) {
          val name = in.readUTF()
          val v = in.readInt()
          if (game != null) detail += (game.worldMap.countries(name) -> v)
        }
        new OperationModifyInfluence(playerId, faction, isAdd, detail.toMap)
      case 2 =>
        new OperationSelectCard(playerId, faction, Option(Cards.fromId(in.readByte())))
      case 3 =>
        val card = Cards.fromId(in.readByte())
        new OperationSelectCardAndAction(playerId, faction, card, Action(in.readInt()))
      case 4 =>
        new OperationSelectOperation(playerId, faction, Action(in.readInt()))
      case 5 =>
        val len = in.readInt()
        val detail = mutable.Set.empty[Country]
        for (i <- 0 until len) {
          val name = in.readUTF()
          if (game != null) detail += game.worldMap.countries(name)
        }
        new OperationSelectCountry(playerId, faction, detail.toSet)
      case 6 =>
        new OperationYesNo(playerId, faction, in.readBoolean())
      case 7 =>
        new OperationIntValue(playerId, faction, in.readInt())
      case 8 =>
        new OperationSelectRegion(playerId, faction, Region(in.readInt()))
      case 9 =>
        val len = in.readInt()
        val cards = mutable.Set.empty[Card]
        for (i <- 0 until len) {
          cards += Cards.fromId(in.readByte())
        }
        new OperationSelectCards(playerId, faction, cards.toSet)
      case 10 =>
        new OperationCubaMissileRequest(playerId, faction, in.readBoolean())
      case _ =>
        null
    }
  }

}
