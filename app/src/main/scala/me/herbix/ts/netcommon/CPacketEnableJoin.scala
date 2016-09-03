package me.herbix.ts.netcommon

import io.netty.buffer.ByteBuf

/**
  * Created by Chaofan on 2016/9/3.
  */
class CPacketEnableJoin(var enable: Boolean) extends Packet {
  def this() = this(false)
  override def encode(byteBuf: ByteBuf): Unit = {
    byteBuf.writeBoolean(enable)
  }
  override def decode(byteBuf: ByteBuf): Unit = {
    enable = byteBuf.readBoolean()
  }
}
