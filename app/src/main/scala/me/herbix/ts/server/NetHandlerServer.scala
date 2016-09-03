package me.herbix.ts.server

import io.netty.channel.{ChannelHandlerContext, SimpleChannelInboundHandler}
import me.herbix.ts.netcommon._

/**
  * Created by Chaofan on 2016/7/3.
  */
class NetHandlerServer extends SimpleChannelInboundHandler[Packet]  {
  val id = Server.nextId()
  var name = "NoName"
  var version = "NoVersion"
  var room: Room = null

  var ctx: ChannelHandlerContext = null

  Server.netHandlers += id -> this

  override def channelActive(ctx: ChannelHandlerContext): Unit = {
    this.ctx = ctx
    ctx.writeAndFlush(new SPacketId(id))
  }

  override def channelRead0(ctx: ChannelHandlerContext, msg: Packet): Unit = {
    msg match {
      case p: CPacketVersion => version(p)
      case p: CPacketRename => rename(p)
      case p: CPacketNewRoom => newRoom(p)
      case p: CPacketJoinRoom => joinRoom(p)
      case p: CPacketLeaveRoom => leaveRoom(p)
      case p: PacketRoomData => roomData(p)
      case p: CPacketExit => exit(p)
      case p: CPacketEnableJoin => enableJoin(p)
    }
  }

  override def exceptionCaught(ctx: ChannelHandlerContext, cause: Throwable): Unit = {
    println(s"$id $cause")
    close()
  }

  def exit(packet: CPacketExit): Unit = {
    println(s"$id exit")
    throw new Exception("exit")
  }

  def rename(packet: CPacketRename): Unit = {
    name = packet.name
    println(s"$id rename $name")
  }

  def version(packet: CPacketVersion): Unit = {
    if (version == "NoVersion") {
      for ((_, room) <- Server.rooms) {
        sendNewRoom(room)
        if (!room.enableJoin) {
          sendEnableJoin(room, room.enableJoin)
        }
      }
    }
    version = packet.version
    println(s"$id changeVersion $version")
  }

  def newRoom(packet: CPacketNewRoom): Unit = {
    println(s"$id newRoom")
    val room = new Room(this)
    if (this.room != null) {
      this.room.leave(this)
    }
    this.room = room
    room.join(this)
    sendJoinRoom(room)
  }

  def joinRoom(packet: CPacketJoinRoom): Unit = {
    val roomId = packet.id
    println(s"$id joinRoom $roomId")
    if (!Server.rooms.contains(roomId)) {
      return
    }
    val room = Server.rooms(roomId)
    if (!room.enableJoin) {
      return
    }
    if (this.room != null) {
      this.room.leave(this)
    }
    this.room = room
    room.join(this)
    sendJoinRoom(room)
  }

  def leaveRoom(packet: CPacketLeaveRoom): Unit = {
    println(s"$id leaveRoom")
    if (room != null) {
      room.leave(this)
    }
  }

  def roomData(packet: PacketRoomData): Unit = {
    val buffer = packet.bytes
    if (room != null) {
      room.roomDataExcept(buffer, this)
    }
  }

  def enableJoin(packet: CPacketEnableJoin): Unit = {
    val enable = packet.enable
    println(s"$id enableJoin $enable")
    if (room != null && room.creator == this) {
      room.enableJoin = enable
      Server.netHandlers.values.foreach(_.sendEnableJoin(room, enable))
    }
  }

  def sendDestroyRoom(room: Room): Unit = {
    ctx.writeAndFlush(new SPacketDestroyRoom(room.id))
  }

  def sendNewRoom(room: Room): Unit = {
    ctx.writeAndFlush(new SPacketNewRoom(room.id, room.creator.id, room.netHandlers.head.name, room.creator.version))
  }

  def sendJoinRoom(room: Room): Unit = {
    ctx.writeAndFlush(new SPacketJoinRoom(room.id, room.netHandlers.map(nh => (nh.id, nh.name)).toList))
  }

  def sendOtherJoinRoom(netHandler: NetHandlerServer): Unit = {
    ctx.writeAndFlush(new SPacketOtherJoinRoom(netHandler.id, netHandler.name))
  }

  def sendLeaveRoom(netHandler: NetHandlerServer): Unit = {
    ctx.writeAndFlush(new SPacketLeaveRoom(netHandler.id))
  }

  def sendData(buffer: Array[Byte]): Unit = {
    ctx.writeAndFlush(new PacketRoomData(buffer))
  }

  def sendEnableJoin(room: Room, enable: Boolean): Unit = {
    ctx.writeAndFlush(new SPacketEnableJoin(room.id, enable))
  }

  def close(): Unit = {
    Server.netHandlers -= this.id
    if (room != null) {
      room.leave(this)
    }
    ctx.close()
    room = null
  }

  override def hashCode = id
}
