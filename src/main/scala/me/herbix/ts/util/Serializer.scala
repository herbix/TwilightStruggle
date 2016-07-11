package me.herbix.ts.util

import java.io.{DataOutputStream, DataInputStream}

import me.herbix.ts.logic.Action.Action
import me.herbix.ts.logic.Faction.Faction
import me.herbix.ts.logic.Region.Region
import me.herbix.ts.logic.State.State
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

  def writeGameState(game: Game, out: DataOutputStream): Unit = {
    import game._
    import Faction._

    out.writeInt(turn)
    out.writeInt(round)
    out.writeInt(phasingPlayer.id)

    out.writeInt(military(US))
    out.writeInt(military(USSR))
    out.writeInt(space(US).level)
    out.writeInt(space(USSR).level)

    out.writeInt(vp)
    out.writeInt(defcon)

    val targetCountries = worldMap.countries.values
      .filter(c => c.influence(US) != 0 || c.influence(USSR) != 0)
    out.writeInt(targetCountries.size)
    for (country <- targetCountries) {
      out.writeUTF(country.name)
      out.writeInt(country.influence(US))
      out.writeInt(country.influence(USSR))
    }

    writeCardSet(hand(US), out)
    writeCardSet(hand(USSR), out)
    writeCardSet(deck, out)
    writeCardSet(discards, out)

    writeFlagSet(flags.flagSets(US), out)
    writeFlagSet(flags.flagSets(USSR), out)
    writeFlagSet(flags.flagSets(Neutral), out)

    writeFlagData(flags.flagData(US), out)
    writeFlagData(flags.flagData(USSR), out)
    writeFlagData(flags.flagData(Neutral), out)

    out.writeBoolean(pendingInput != null)
    if (pendingInput != null) {
      pendingInput.writeToStream(out)
    }

    writeStack[State](stateStack, out, s => out.writeInt(s.id))
    writeStack[Faction](operatingPlayerStack, out, f => out.writeInt(f.id))
    writeStack[Card](currentCardStack, out, c => out.writeInt(c.id))
    writeStack[Any](currentCardDataStack, out, a => writeData(a, out))

    out.writeInt(currentRealignments.size)
    for (country <- currentRealignments) {
      out.writeUTF(country.name)
    }

    out.writeBoolean(skipHeadlineCard2)
  }

  def readGameState(game: Game, in: DataInputStream): Unit = {

  }

  def writeCardSet(cards: CardSet, out: DataOutputStream): Unit = {
    out.writeInt(cards.cardCount)
    for (card <- cards) {
      out.writeInt(card.id)
    }
  }

  def writeFlagSet(flags: mutable.Set[Flag], out: DataOutputStream): Unit = {
    out.writeInt(flags.size)
    for (flag <- flags) {
      out.writeInt(flag.id)
    }
  }

  def writeFlagData(data: mutable.Map[Flag, Any], out: DataOutputStream): Unit = {
    val target = data.filter(_._2 != null)
    out.writeInt(target.size)
    for ((flag, data) <- target) {
      out.writeInt(flag.id)
      writeData(data, out)
    }
  }

  def writeStack[T](stack: mutable.Stack[T], out: DataOutputStream, cb: (T) => Unit): Unit = {
    out.writeInt(stack.size)
    for (item <- stack.reverseIterator) {
      cb(item)
    }
  }

  def writeData(data: Any, out: DataOutputStream): Unit = {
    data match {
      case null =>
        out.writeByte(0)
      case int: Int =>
        out.writeByte(1)
        out.writeInt(int)
      case boolean: Boolean =>
        out.writeByte(2)
        out.writeBoolean(boolean)
      case region: Region =>
        out.writeByte(3)
        out.writeInt(region.id)
      case cardSet: CardSet =>
        out.writeByte(4)
        writeCardSet(cardSet, out)
      case card: Card =>
        out.writeByte(5)
        out.writeInt(card.id)
      case (int: Int, country: Country) =>
        out.writeByte(6)
        out.writeInt(int)
        out.writeUTF(country.name)

    }
  }
}
