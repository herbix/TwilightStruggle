// Copyright (C) 2017 Chaofan

package me.herbix.ts.netcommon

import io.netty.buffer.ByteBuf

/**
  * Created by Chaofan on 2016/9/3.
  */
class SPacketNewRoom(var id: Int, var creator: Int, var name: String, var version: String) extends Packet {
  def this() = this(0, 0, "", "")
  override def encode(byteBuf: ByteBuf): Unit = {
    byteBuf.writeInt(id)
    byteBuf.writeInt(creator)
    Packet.writeUTF(byteBuf, name)
    Packet.writeUTF(byteBuf, version)
  }
  override def decode(byteBuf: ByteBuf): Unit = {
    id = byteBuf.readInt()
    creator = byteBuf.readInt()
    name = Packet.readUTF(byteBuf)
    version = Packet.readUTF(byteBuf)
  }
}
