package me.herbix.ts.logic

import java.io.DataOutputStream

import me.herbix.ts.logic.Action.Action
import me.herbix.ts.logic.Faction.Faction
import me.herbix.ts.logic.Region.Region

/**
  * Created by Chaofan on 2016/6/18.
  */
abstract class Operation(val playerId: Int, val faction: Faction) {
  def writeToStream(out: DataOutputStream) = {
    out.writeInt(playerId)
    out.writeInt(faction.id)
    out.writeByte(opId)
    writeData(out)
  }
  protected val opId: Byte
  protected def writeData(out: DataOutputStream)
}

class OperationChooseFaction(playerId: Int, faction: Faction) extends Operation(playerId, faction) {
  override protected val opId: Byte = 0
  override protected def writeData(out: DataOutputStream): Unit = {}
}

class OperationModifyInfluence(playerId: Int, faction: Faction, val isAdd: Boolean, val detail: Map[Country, Int])
  extends Operation(playerId, faction) {
  override protected val opId: Byte = 1
  override protected def writeData(out: DataOutputStream): Unit = {
    out.writeBoolean(isAdd)
    out.writeInt(detail.size)
    for ((c, i) <- detail) {
      out.writeUTF(c.name)
      out.writeInt(i)
    }
  }
}

class OperationSelectCard(playerId: Int, faction: Faction, val card: Option[Card]) extends Operation(playerId, faction) {
  override protected val opId: Byte = 2
  override protected def writeData(out: DataOutputStream): Unit = {
    out.writeByte(card.map(_.id).getOrElse(-1))
  }
}

class OperationSelectCardAndAction(playerId: Int, faction: Faction, val card: Card, val action: Action)
  extends Operation(playerId, faction) {
  override protected val opId: Byte = 3
  override protected def writeData(out: DataOutputStream): Unit = {
    out.writeByte(if (card != null) card.id else -1)
    out.writeInt(action.id)
  }
}

class OperationSelectOperation(playerId: Int, faction: Faction, val action: Action) extends Operation(playerId, faction) {
  override protected val opId: Byte = 4
  override protected def writeData(out: DataOutputStream): Unit = {
    out.writeInt(action.id)
  }
}

class OperationSelectCountry(playerId: Int, faction: Faction, val detail: Set[Country]) extends Operation(playerId, faction) {
  override protected val opId: Byte = 5
  override protected def writeData(out: DataOutputStream): Unit = {
    out.writeInt(detail.size)
    for (c <- detail) {
      out.writeUTF(c.name)
    }
  }
}

class OperationYesNo(playerId: Int, faction: Faction, val value: Boolean) extends Operation(playerId, faction) {
  override protected val opId: Byte = 6
  override protected def writeData(out: DataOutputStream): Unit = {
    out.writeBoolean(value)
  }
}

class OperationIntValue(playerId: Int, faction: Faction, val value: Int) extends Operation(playerId, faction) {
  override protected val opId: Byte = 7
  override protected def writeData(out: DataOutputStream): Unit = {
    out.writeInt(value)
  }
}

class OperationSelectRegion(playerId: Int, faction: Faction, val region: Region) extends Operation(playerId, faction) {
  override protected val opId: Byte = 8
  override protected def writeData(out: DataOutputStream): Unit = {
    out.writeInt(region.id)
  }
}

class OperationSelectCards(playerId: Int, faction: Faction, val cards: Set[Card]) extends Operation(playerId, faction) {
  override protected val opId: Byte = 9
  override protected def writeData(out: DataOutputStream): Unit = {
    out.writeInt(cards.size)
    for (card <- cards) {
      out.writeByte(card.id)
    }
  }
}

class OperationCubaMissileRequest(playerId: Int, faction: Faction, val isResponse: Boolean) extends Operation(playerId, faction) {
  override protected val opId: Byte = 10
  override protected def writeData(out: DataOutputStream): Unit = {
    out.writeBoolean(isResponse)
  }
}
