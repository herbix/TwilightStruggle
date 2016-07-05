package me.herbix.ts.logic

import java.io.DataOutputStream

import me.herbix.ts.logic.Action.Action
import me.herbix.ts.logic.Faction.Faction
import me.herbix.ts.logic.Region.Region

import me.herbix.ts.util.Serializer._

/**
  * Created by Chaofan on 2016/6/18.
  */
abstract class Operation(val playerId: Int, val faction: Faction) {
  def writeToStream(out: DataOutputStream) = {
    out.writeInt(playerId)
    out.writeInt(faction)
    out.writeByte(opId)
    writeData(out)
  }
  val opId: Byte
  def writeData(out: DataOutputStream)
}

class OperationChooseFaction(playerId: Int, faction: Faction) extends Operation(playerId, faction) {
  override val opId: Byte = 0
  override def writeData(out: DataOutputStream): Unit = {}
}

class OperationModifyInfluence(playerId: Int, faction: Faction, val isAdd: Boolean, val detail: Map[Country, Int])
  extends Operation(playerId, faction) {
  override val opId: Byte = 1
  override def writeData(out: DataOutputStream): Unit = {
    out.writeBoolean(isAdd)
    out.writeInt(detail.size)
    for ((c, i) <- detail) {
      out.writeUTF(c.name)
      out.writeInt(i)
    }
  }
}

class OperationSelectCard(playerId: Int, faction: Faction, val card: Card) extends Operation(playerId, faction) {
  override val opId: Byte = 2
  override def writeData(out: DataOutputStream): Unit = {
    out.writeByte(card.id)
  }
}

class OperationSelectCardAndAction(playerId: Int, faction: Faction, card: Card, val action: Action)
  extends OperationSelectCard(playerId, faction, card) {
  override val opId: Byte = 3
  override def writeData(out: DataOutputStream): Unit = {
    super.writeData(out)
    out.writeInt(action)
  }
}

class OperationSelectOperation(playerId: Int, faction: Faction, val action: Action) extends Operation(playerId, faction) {
  override val opId: Byte = 4
  override def writeData(out: DataOutputStream): Unit = {
    out.writeInt(action)
  }
}

class OperationSelectCountry(playerId: Int, faction: Faction, val detail: Set[Country]) extends Operation(playerId, faction) {
  override val opId: Byte = 5
  override def writeData(out: DataOutputStream): Unit = {
    out.writeInt(detail.size)
    for (c <- detail) {
      out.writeUTF(c.name)
    }
  }
}

class OperationYesNo(playerId: Int, faction: Faction, val value: Boolean) extends Operation(playerId, faction) {
  override val opId: Byte = 6
  override def writeData(out: DataOutputStream): Unit = {
    out.writeBoolean(value)
  }
}

class OperationIntValue(playerId: Int, faction: Faction, val value: Int) extends Operation(playerId, faction) {
  override val opId: Byte = 7
  override def writeData(out: DataOutputStream): Unit = {
    out.writeInt(value)
  }
}

class OperationSelectRegion(playerId: Int, faction: Faction, val region: Region) extends Operation(playerId, faction) {
  override val opId: Byte = 8
  override def writeData(out: DataOutputStream): Unit = {
    out.writeInt(region)
  }
}

class OperationSelectCards(playerId: Int, faction: Faction, val cards: Set[Card]) extends Operation(playerId, faction) {
  override val opId: Byte = 9
  override def writeData(out: DataOutputStream): Unit = {
    out.writeInt(cards.size)
    for (card <- cards) {
      out.writeInt(card.id)
    }
  }
}

class OperationCubaMissileRequest(playerId: Int, faction: Faction, val isResponse: Boolean) extends Operation(playerId, faction) {
  override val opId: Byte = 10
  override def writeData(out: DataOutputStream): Unit = {
    out.writeBoolean(isResponse)
  }
}
