package me.herbix.ts.netcommon

import java.util

import io.netty.buffer.ByteBuf
import io.netty.channel.ChannelHandlerContext
import io.netty.handler.codec.{ByteToMessageCodec, MessageToMessageCodec}

/**
  * Created by Chaofan on 2016/9/3.
  */
class NetCodec extends ByteToMessageCodec[Packet] {

  override def encode(ctx: ChannelHandlerContext, packet: Packet, buffer: ByteBuf): Unit = {
    buffer.writeByte(Packet.getIdByInstance(packet))
    packet.encode(buffer)
  }

  override def decode(ctx: ChannelHandlerContext, buffer: ByteBuf, out: util.List[AnyRef]): Unit = {
    buffer.markReaderIndex()

    val id = buffer.readByte()
    val packet = Packet.getInstanceById(id)

    try {
      packet.decode(buffer)
      out.add(packet)
    } catch {
      case ex: IndexOutOfBoundsException =>
        buffer.resetReaderIndex()
    }
  }

}
