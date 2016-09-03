package me.herbix.ts.netcommon

import java.util

import io.netty.buffer.ByteBuf
import io.netty.channel.ChannelHandlerContext
import io.netty.handler.codec.{ByteToMessageCodec, MessageToMessageCodec}

/**
  * Created by Chaofan on 2016/9/3.
  */
class NetCodec extends ByteToMessageCodec[Packet] {

  override def encode(ctx: ChannelHandlerContext, msg: Packet, buffer: ByteBuf): Unit = {
    buffer.writeByte(Packet.getIdByInstance(msg))
    msg.encode(buffer)
  }

  override def decode(ctx: ChannelHandlerContext, msg: ByteBuf, out: util.List[AnyRef]): Unit = {
    val id = msg.readByte()
    val packet = Packet.getInstanceById(id)
    packet.decode(msg)
    out.add(packet)
  }

}
