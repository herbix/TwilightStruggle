package me.herbix.ts.netcommon

import io.netty.buffer.ByteBuf

/**
  * Created by Chaofan on 2016/9/3.
  */
class SPacketEnableJoin(var id: Int, var enable: Boolean) extends Packet {
  def this() = this(0, false)
  override def encode(byteBuf: ByteBuf): Unit = {
    byteBuf.writeInt(id)
    byteBuf.writeBoolean(enable)
  }
  override def decode(byteBuf: ByteBuf): Unit = {
    id = byteBuf.readInt()
    enable = byteBuf.readBoolean()
  }
}
