package me.herbix.ts.netcommon

import io.netty.buffer.ByteBuf

import scala.collection.mutable

/**
  * Created by Chaofan on 2016/9/3.
  */
trait Packet {
  def encode(byteBuf: ByteBuf): Unit
  def decode(byteBuf: ByteBuf): Unit
}

object Packet {
  private var id: Byte = 0
  private val idByType = mutable.Map.empty[Class[_ <: Packet], Byte]
  private val typeById = mutable.Map.empty[Byte, Class[_ <: Packet]]

  registerPacketType(classOf[PacketRoomData])

  id = 20
  registerPacketType(classOf[SPacketId])
  registerPacketType(classOf[SPacketNewRoom])
  registerPacketType(classOf[SPacketJoinRoom])
  registerPacketType(classOf[SPacketOtherJoinRoom])
  registerPacketType(classOf[SPacketLeaveRoom])
  registerPacketType(classOf[SPacketDestroyRoom])
  registerPacketType(classOf[SPacketEnableJoin])

  id = 40
  registerPacketType(classOf[CPacketVersion])
  registerPacketType(classOf[CPacketRename])
  registerPacketType(classOf[CPacketNewRoom])
  registerPacketType(classOf[CPacketJoinRoom])
  registerPacketType(classOf[CPacketLeaveRoom])
  registerPacketType(classOf[CPacketExit])
  registerPacketType(classOf[CPacketEnableJoin])

  private def registerPacketType(classType: Class[_ <: Packet]): Int = {
    val typeId = id

    idByType += classType -> typeId
    typeById += typeId -> classType

    id = (id + 1).toByte
    typeId
  }

  def getIdByInstance(packet: Packet): Int = {
    idByType(packet.getClass)
  }

  def getInstanceById(id: Byte): Packet = {
    typeById(id).newInstance()
  }

  def writeUTF(byteBuf: ByteBuf, string: String): Unit = {
    byteBuf.writeBoolean(string != null)
    if (string != null) {
      val bytes = string.getBytes("UTF-8")
      byteBuf.writeInt(bytes.length)
      byteBuf.writeBytes(bytes)
    }
  }

  def readUTF(byteBuf: ByteBuf): String = {
    if (byteBuf.readBoolean()) {
      val len = byteBuf.readInt()
      val bytes = new Array[Byte](len)
      byteBuf.readBytes(bytes)
      new String(bytes, "UTF-8")
    } else {
      null
    }
  }
}
