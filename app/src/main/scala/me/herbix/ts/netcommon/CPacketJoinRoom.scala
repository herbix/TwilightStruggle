package me.herbix.ts.netcommon

import io.netty.buffer.ByteBuf

/**
  * Created by Chaofan on 2016/9/3.
  */
class CPacketJoinRoom(var id: Int) extends Packet {
  def this() = this(0)
  override def encode(byteBuf: ByteBuf): Unit = {
    byteBuf.writeInt(id)
  }
  override def decode(byteBuf: ByteBuf): Unit = {
    id = byteBuf.readInt()
  }
}
