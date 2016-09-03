package me.herbix.ts.netcommon

import io.netty.buffer.ByteBuf

/**
  * Created by Chaofan on 2016/9/3.
  */
class PacketRoomData(var bytes: Array[Byte]) extends Packet {
  def this() = this(null)
  override def encode(byteBuf: ByteBuf): Unit = {
    byteBuf.writeInt(bytes.length)
    byteBuf.writeBytes(bytes)
  }
  override def decode(byteBuf: ByteBuf): Unit = {
    val len = byteBuf.readInt()
    bytes = new Array[Byte](len)
    byteBuf.readBytes(bytes)
  }
}
