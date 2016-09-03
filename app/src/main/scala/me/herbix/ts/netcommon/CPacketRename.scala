package me.herbix.ts.netcommon

import io.netty.buffer.ByteBuf

/**
  * Created by Chaofan on 2016/9/3.
  */
class CPacketRename(var name: String) extends Packet {
  def this() = this("")
  override def encode(byteBuf: ByteBuf): Unit = {
    Packet.writeUTF(byteBuf, name)
  }
  override def decode(byteBuf: ByteBuf): Unit = {
    name = Packet.readUTF(byteBuf)
  }
}
