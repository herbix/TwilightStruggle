// Copyright (C) 2017 Chaofan

package me.herbix.ts.netcommon

import io.netty.buffer.ByteBuf

/**
  * Created by Chaofan on 2016/9/3.
  */
class SPacketJoinRoom(var id: Int, var members: List[(Int, String)]) extends Packet {
  def this() = this(0, List.empty)
  override def encode(byteBuf: ByteBuf): Unit = {
    byteBuf.writeInt(id)
    byteBuf.writeInt(members.length)
    for ((id, name) <- members) {
      byteBuf.writeInt(id)
      Packet.writeUTF(byteBuf, name)
    }
  }
  override def decode(byteBuf: ByteBuf): Unit = {
    id = byteBuf.readInt()
    val len = byteBuf.readInt()
    for (i <- 1 to len) {
      val id = byteBuf.readInt()
      val name = Packet.readUTF(byteBuf)
      members = members :+ (id, name)
    }
  }
}
