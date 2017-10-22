// Copyright (C) 2017 Chaofan

package me.herbix.ts.netcommon

import io.netty.buffer.ByteBuf

/**
  * Created by Chaofan on 2016/9/3.
  */
class CPacketVersion(var version: String) extends Packet {
  def this() = this("")
  override def encode(byteBuf: ByteBuf): Unit = {
    Packet.writeUTF(byteBuf, version)
  }
  override def decode(byteBuf: ByteBuf): Unit = {
    version = Packet.readUTF(byteBuf)
  }
}
