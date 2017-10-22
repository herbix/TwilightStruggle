// Copyright (C) 2017 Chaofan

package me.herbix.ts.netcommon

import io.netty.buffer.ByteBuf

/**
  * Created by Chaofan on 2016/9/3.
  */
class SPacketOtherJoinRoom(var id: Int, var name: String) extends Packet {
  def this() = this(0, null)
  override def encode(byteBuf: ByteBuf): Unit = {
    byteBuf.writeInt(id)
    Packet.writeUTF(byteBuf, name)
  }
  override def decode(byteBuf: ByteBuf): Unit = {
    id = byteBuf.readInt()
    name = Packet.readUTF(byteBuf)
  }
}
