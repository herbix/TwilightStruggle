// Copyright (C) 2017 Chaofan

package me.herbix.ts.netcommon

import io.netty.buffer.ByteBuf

/**
  * Created by Chaofan on 2016/9/3.
  */
class CPacketLeaveRoom extends Packet {
  override def encode(byteBuf: ByteBuf): Unit = {}
  override def decode(byteBuf: ByteBuf): Unit = {}
}
