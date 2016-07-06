package me.herbix.ts.util

import java.io.DataInputStream

import me.herbix.ts.logic.Action.Action
import me.herbix.ts.logic.Action.Action
import me.herbix.ts.logic.Region.Region
import me.herbix.ts.logic.Region.Region
import me.herbix.ts.logic._
import me.herbix.ts.logic.Faction.Faction

import scala.collection.mutable

/**
  * Created by Chaofan on 2016/7/5.
  */
object Serializer {

  implicit def FactionToInt(faction: Faction): Int =
    faction match {
      case Faction.Neutral => 0
      case Faction.US => 1
      case Faction.USSR => 2
    }

  implicit def IntToFaction(faction: Int): Faction =
    faction match {
      case 0 => Faction.Neutral
      case 1 => Faction.US
      case 2 => Faction.USSR
    }

  implicit def ActionToInt(action: Action): Int =
    action match {
      case Action.Space => 0
      case Action.Event => 1
      case Action.Operation => 2
      case Action.Influence => 3
      case Action.Realignment => 4
      case Action.Coup => 5
    }

  implicit def IntToAction(action: Int): Action =
    action match {
      case 0 => Action.Space
      case 1 => Action.Event
      case 2 => Action.Operation
      case 3 => Action.Influence
      case 4 => Action.Realignment
      case 5 => Action.Coup
    }

  implicit def RegionToInt(region: Region): Int =
    region match {
      case Region.Europe => 0
      case Region.Asia => 1
      case Region.MidEast => 2
      case Region.Africa => 3
      case Region.MidAmerica => 4
      case Region.SouthAmerica => 5
    }

  implicit def IntToRegion(region: Int): Region =
    region match {
      case 0 => Region.Europe
      case 1 => Region.Asia
      case 2 => Region.MidEast
      case 3 => Region.Africa
      case 4 => Region.MidAmerica
      case 5 => Region.SouthAmerica
    }


  def readOperation(in: DataInputStream, game: Game): Operation = {
    val playerId = in.readInt()
    val faction: Faction = in.readInt()
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
        new OperationSelectCardAndAction(playerId, faction, card, in.readInt())
      case 4 =>
        new OperationSelectOperation(playerId, faction, in.readInt())
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
        new OperationSelectRegion(playerId, faction, in.readInt())
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
