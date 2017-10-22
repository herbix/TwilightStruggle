// Copyright (C) 2017 Chaofan

package me.herbix.ts.util

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, DataInputStream, DataOutputStream}

import me.herbix.ts.logic.Faction._
import me.herbix.ts.logic.Region.Region
import me.herbix.ts.logic.State.State
import me.herbix.ts.logic._
import me.herbix.ts.logic.card.Card
import me.herbix.ts.logic.turnzero.{Crisis, GameTurnZero}

import scala.collection.mutable

/**
  * Created by Chaofan on 2016/7/5.
  */
object Serializer {

  def readOperation(in: DataInputStream)(implicit game: Game): Operation = {
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
          val id = in.readByte()
          val v = in.readInt()
          detail += (game.theWorldMap.getCountryFromId(id) -> v)
        }
        new OperationModifyInfluence(playerId, faction, isAdd, detail.toMap)
      case 2 =>
        new OperationSelectCard(playerId, faction, Option(game.theCards.fromId(in.readInt())))
      case 3 =>
        val card = game.theCards.fromId(in.readInt())
        new OperationSelectCardAndAction(playerId, faction, card, Action(in.readInt()))
      case 4 =>
        new OperationSelectOperation(playerId, faction, Action(in.readInt()))
      case 5 =>
        val len = in.readInt()
        val detail = mutable.Set.empty[Country]
        for (i <- 0 until len) {
          val id = in.readByte()
          detail += game.theWorldMap.getCountryFromId(id)
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
          cards += game.theCards.fromId(in.readInt())
        }
        new OperationSelectCards(playerId, faction, cards.toSet)
      case 10 =>
        new OperationCubaMissileRequest(playerId, faction, in.readBoolean())
      case _ =>
        null
    }
  }

  def writeGameState(implicit game: Game, out: DataOutputStream): Unit = {
    import game._

    out.writeByte(turn)
    out.writeByte(round)
    out.writeByte(phasingPlayer.id)

    out.writeByte(military(US))
    out.writeByte(military(USSR))
    out.writeByte(space(US).level)
    out.writeByte(space(USSR).level)

    out.writeInt(vp)
    out.writeByte(defcon)

    val targetCountries = game.theWorldMap.countries.values
      .filter(c => influence(c, US) != 0 || influence(c, USSR) != 0)
    out.writeInt(targetCountries.size)
    for (country <- targetCountries) {
      out.writeByte(country.id)
      out.writeShort(influence(country, US))
      out.writeShort(influence(country, USSR))
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
    writeStack[Faction](operatingPlayerStack, out, f => out.writeByte(f.id))
    writeStack[Card](currentCardStack, out, c => out.writeInt(c.id))
    writeStack[Any](currentCardDataStack, out, a => writeData(a, game, out))

    out.writeInt(currentRealignments.size)
    for (country <- currentRealignments) {
      out.writeUTF(country.name)
    }

    out.writeBoolean(skipHeadlineCard2)

    game match {
      case g: GameTurnZero =>
        for (i <- 0 until 6) {
          val crisis = g.crisisDeck(i)
          if (crisis == null) {
            out.writeByte(-1)
          } else {
            out.writeByte(crisis.id)
          }
          out.writeByte(g.crisisEffect(i))
        }
        if (g.currentSolvingFlag == null) {
          out.writeInt(-1)
        } else {
          out.writeInt(g.currentSolvingFlag.id)
        }
      case _ =>
    }
  }

  def readGameState(implicit game: Game, in: DataInputStream): Unit = {
    import game._

    game.turn = in.readByte()
    game.round = in.readByte()
    game.phasingPlayer = Faction(in.readByte())

    game.military(US) = in.readByte()
    game.military(USSR) = in.readByte()
    game.space(US) = SpaceLevel(in.readByte())
    game.space(USSR) = SpaceLevel(in.readByte())

    game.vp = in.readInt()
    game.defcon = in.readByte()

    for (c <- game.theWorldMap.countries.values) {
      game.countryInfluence(c)(US) = 0
      game.countryInfluence(c)(USSR) = 0
    }
    val countryCount = in.readInt()
    for (i <- 1 to countryCount) {
      val id = in.readByte()
      val country = game.theWorldMap.getCountryFromId(id)
      game.countryInfluence(country)(US) = in.readShort()
      game.countryInfluence(country)(USSR) = in.readShort()
    }

    readCardSet(hand(US), in)
    readCardSet(hand(USSR), in)
    readCardSet(deck, in)
    readCardSet(discards, in)

    readFlagSet(flags.flagSets(US), in)
    readFlagSet(flags.flagSets(USSR), in)
    readFlagSet(flags.flagSets(Neutral), in)

    flags.flagSets2(US).clear()
    flags.flagSets2(US) ++= flags.flagSets(US)
    flags.flagSets2(US) ++= flags.flagSets(Neutral)

    flags.flagSets2(USSR).clear()
    flags.flagSets2(USSR) ++= flags.flagSets(USSR)
    flags.flagSets2(USSR) ++= flags.flagSets(Neutral)

    readFlagData(flags.flagData(US), flags.flagSets(US), in)
    readFlagData(flags.flagData(USSR), flags.flagSets(USSR), in)
    readFlagData(flags.flagData(Neutral), flags.flagSets(Neutral), in)

    if (in.readBoolean()) {
      pendingInput = readOperation(in)
    }

    readStack[State](stateStack, in, State(in.readInt()))
    readStack[Faction](operatingPlayerStack, in, Faction(in.readByte()))
    readStack[Card](currentCardStack, in, game.theCards.fromId(in.readInt()))
    readStack[Any](currentCardDataStack, in, readData(in, game))

    currentRealignments = List.empty
    val currentRealignmentSize = in.readInt()
    for (i <- 1 to currentRealignmentSize) {
      currentRealignments :+= game.theWorldMap.countries(in.readUTF())
    }

    skipHeadlineCard2 = in.readBoolean()

    game match {
      case g: GameTurnZero =>
        for (i <- 0 until 6) {
          val crisisId = in.readByte()
          g.crisisDeck(i) = if (crisisId < 0) null else Crisis.fromId(crisisId)
          g.crisisEffect(i) = in.readByte()
        }
        val currentSolvingFlagId: Int = in.readInt()
        g.currentSolvingFlag = if (currentSolvingFlagId < 0) null else FlagsTrait.fromId(currentSolvingFlagId)
      case _ =>
    }
  }

  def writeGameState(game: Game): Array[Byte] = {
    val out = new ByteArrayOutputStream
    writeGameState(game, new DataOutputStream(out))
    out.toByteArray
  }

  def readGameState(game: Game, data: Array[Byte]): Unit = {
    readGameState(game, new DataInputStream(new ByteArrayInputStream(data)))
  }

  def copyGameState(to: Game, from: Game): Unit = {
    val out = new ByteArrayOutputStream
    writeGameState(from, new DataOutputStream(out))
    readGameState(to, new DataInputStream(new ByteArrayInputStream(out.toByteArray)))
  }

  def writeGameRecordingHistoryState(implicit game: GameRecordingHistory, out: DataOutputStream): Unit = {
    writeGameState(game, out)

    import game._

    out.writeInt(currentHistoryId)
    currentHistory.lastOption match {
      case Some(last) =>
        out.writeInt(last.id)
      case None =>
        out.writeInt(-1)
    }
  }

  def readGameRecordingHistoryState(implicit game: GameRecordingHistory, in: DataInputStream): Unit = {
    readGameState(game, in)

    import game._

    currentHistoryId = in.readInt()

    val lastHistoryId = in.readInt()
    if (lastHistoryId >= 0) {
      adjustHistory(lastHistoryId)
    }
  }

  def adjustHistory(lastHistoryId: Int)(implicit game: GameRecordingHistory): Unit = {
    import game._

    val pos = currentHistory.indexWhere(_.id == lastHistoryId)
    if (pos >= 0) {
      currentHistory = currentHistory.slice(0, pos + 1)
    } else {
      val pos2 = oldHistory.indexWhere(_.id == lastHistoryId)
      if (pos2 >= 0) {
        val isCut = (h: History) => h.isInstanceOf[HistoryTurnRound] || h.isInstanceOf[HistoryStartGame]
        val pos3 = oldHistory.lastIndexWhere(isCut, pos2)
        val pos4 = oldHistory.indexWhere(isCut, pos2 + 1)

        if (pos3 >= 0) {
          currentHistory = oldHistory.slice(pos3, pos2 + 1)
        } else {
          currentHistory = oldHistory.slice(0, pos2 + 1)
        }
        if (pos4 >= 0) {
          oldHistory = oldHistory.slice(pos4, oldHistory.size)
        } else {
          oldHistory = List.empty
        }
      }
    }
  }

  def writeCardSet(cards: CardSet, out: DataOutputStream): Unit = {
    out.writeInt(cards.cardCount)
    for (card <- cards) {
      out.writeInt(card.id)
    }
  }

  def readCardSet(cards: CardSet, in: DataInputStream)(implicit game: Game): Unit = {
    cards.clear()
    val len = in.readInt()
    for (i <- 1 to len) {
      val card = game.theCards.fromId(in.readInt())
      cards.add(card)
    }
  }

  def writeFlagSet(flags: mutable.Set[Flag], out: DataOutputStream): Unit = {
    out.writeInt(flags.size)
    for (flag <- flags) {
      out.writeByte(flag.id)
    }
  }

  def readFlagSet(flags: mutable.Set[Flag], in: DataInputStream): Unit = {
    flags.clear()
    val len = in.readInt()
    for (i <- 1 to len) {
      val flag = FlagsTrait.fromId(in.readByte())
      flags.add(flag)
    }
  }

  def writeFlagData(dataSet: mutable.Map[Flag, Any], out: DataOutputStream)(implicit game: Game): Unit = {
    val target = dataSet.filter(_._2 != null)
    out.writeInt(target.size)
    for ((flag, data) <- target) {
      out.writeByte(flag.id)
      writeData(data, game, out)
    }
  }

  def readFlagData(dataSet: mutable.Map[Flag, Any], flags: mutable.Set[Flag], in: DataInputStream)(implicit game: Game): Unit = {
    dataSet.clear()
    flags.foreach(flag => dataSet(flag) = null)
    val len = in.readInt()
    for (i <- 1 to len) {
      val flag = FlagsTrait.fromId(in.readByte())
      val data = readData(in, game)
      dataSet(flag) = data
    }
  }

  def writeStack[T](stack: mutable.Stack[T], out: DataOutputStream, cb: (T) => Unit): Unit = {
    out.writeInt(stack.size)
    for (item <- stack.reverseIterator) {
      cb(item)
    }
  }

  def readStack[T](stack: mutable.Stack[T], in: DataInputStream, cb: => T): Unit = {
    stack.clear()
    val len = in.readInt()
    for (i <- 1 to len) {
      stack.push(cb)
    }
  }

  def writeData(data: Any, game: Game, out: DataOutputStream): Unit = {
    val usHand = game.hand(US)
    val ussrHand = game.hand(USSR)
    data match {
      case null =>
        out.writeByte(0)
      case `usHand` =>
        out.writeByte(7)
      case `ussrHand` =>
        out.writeByte(8)
      case game.discards =>
        out.writeByte(9)
      case int: Int =>
        out.writeByte(1)
        out.writeInt(int)
      case boolean: Boolean =>
        out.writeByte(2)
        out.writeBoolean(boolean)
      case region: Region =>
        out.writeByte(3)
        out.writeByte(region.id)
      case cardSet: CardSet =>
        out.writeByte(4)
        writeCardSet(cardSet, out)
      case card: Card =>
        out.writeByte(5)
        out.writeInt(card.id)
      case (int: Int, country: Country) =>
        out.writeByte(6)
        out.writeInt(int)
        out.writeByte(country.id)
    }
  }

  def readData(in: DataInputStream, game: Game): Any = {
    in.readByte() match {
      case 0 => null
      case 1 => in.readInt()
      case 2 => in.readBoolean()
      case 3 => Region(in.readByte())
      case 4 =>
        val cardSet = new CardSet(game)
        readCardSet(cardSet, in)(game)
        cardSet
      case 5 => game.theCards.fromId(in.readInt())
      case 6 =>
        val int = in.readInt()
        val country = game.theWorldMap.getCountryFromId(in.readByte())
        (int, country)
      case 7 => game.hand(US)
      case 8 => game.hand(USSR)
      case 9 => game.discards
    }
  }

}
