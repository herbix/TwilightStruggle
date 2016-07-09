package me.herbix.ts.server

import java.io.{DataOutputStream, DataInputStream}
import java.net.Socket

/**
  * Created by Chaofan on 2016/7/3.
  */
class NetHandlerServer(socket: Socket) {
  val id = Server.nextId()
  var name = "NoName"
  var version = "legacy"
  var room: Room = null

  Server.netHandlers += id -> this

  val in = new DataInputStream(socket.getInputStream)
  val out = new DataOutputStream(socket.getOutputStream)

  out.writeByte(0)
  out.writeInt(id)

  new Thread(){
    override def run(): Unit = {
      try {
        while (true) {
          val b = in.readByte()
          b match {
            case 0 => exit()
            case 1 => rename()
            case 2 => newRoom()
            case 3 => joinRoom()
            case 4 => leaveRoom()
            case 5 => roomData()
            case 6 => changeVersion()
          }
        }
      } catch {
        case e: Throwable =>
          e.printStackTrace()
          close()
      }
    }
  }.start()

  def exit(): Unit = {
    println(s"$id exit")
    throw new Exception("exit")
  }

  def rename(): Unit = {
    if (name == "NoName") {
      for ((_, room) <- Server.rooms) {
        sendNewRoom(room)
      }
    }
    name = in.readUTF()
    println(s"$id rename $name")
  }

  def changeVersion(): Unit = {
    version = in.readUTF()
    println(s"$id changeVersion $name")
  }

  def newRoom(): Unit = {
    println(s"$id newRoom")
    val room = new Room(this)
    if (this.room != null) {
      this.room.leave(this)
    }
    this.room = room
    room.join(this)
    sendJoinRoom(room)
  }

  def joinRoom(): Unit = {
    val roomId = in.readInt()
    println(s"$id joinRoom $roomId")
    if (!Server.rooms.contains(roomId)) {
      return
    }
    val room = Server.rooms(roomId)
    if (this.room != null) {
      this.room.leave(this)
    }
    this.room = room
    room.join(this)
    sendJoinRoom(room)
  }

  def leaveRoom(): Unit = {
    println(s"$id leaveRoom")
    if (room != null) {
      room.leave(this)
    }
  }

  def roomData(): Unit = {
    val length = in.readInt()
    val buffer = new Array[Byte](length)
    in.readFully(buffer)
    if (room != null) {
      room.roomDataExcept(buffer, this)
    }
  }

  def sendDestroyRoom(room: Room): Unit = {
    this.synchronized {
      out.writeByte(1)
      out.writeInt(room.id)
    }
  }

  def sendNewRoom(room: Room): Unit = {
    this.synchronized {
      out.writeByte(2)
      out.writeInt(room.id)
      out.writeInt(room.creator.id)
      out.writeUTF(room.netHandlers.head.name)
      if (version != "legacy") {
        out.writeUTF(room.creator.version)
      }
    }
  }

  def sendJoinRoom(room: Room): Unit = {
    this.synchronized {
      out.writeByte(3)
      out.writeInt(room.id)
      out.writeInt(room.netHandlers.size)
      for (nh <- room.netHandlers) {
        out.writeInt(nh.id)
        out.writeUTF(nh.name)
      }
    }
  }

  def sendOtherJoinRoom(netHandler: NetHandlerServer): Unit = {
    this.synchronized {
      out.writeByte(6)
      out.writeInt(netHandler.id)
      out.writeUTF(netHandler.name)
    }
  }

  def sendLeaveRoom(netHandler: NetHandlerServer): Unit = {
    this.synchronized {
      out.writeByte(4)
      out.writeInt(netHandler.id)
    }
  }

  def sendData(buffer: Array[Byte]): Unit = {
    this.synchronized {
      out.writeByte(5)
      out.writeInt(buffer.length)
      out.write(buffer)
    }
  }

  def close(): Unit = {
    Server.netHandlers -= this.id
    if (room != null) {
      room.leave(this)
    }
    try {
      socket.close()
    } catch {
      case e: Throwable =>
    }
    room = null
  }

  override def hashCode = id
}
