package me.herbix.ts.client

import java.io.{DataOutputStream, DataInputStream}
import java.net.Socket

import scala.util.Random

/**
  * Created by Chaofan on 2016/7/3.
  */
class NetHandlerClient(socket: Socket) {
  var id = 0
  var name: String = "TS-" + Integer.toHexString(Random.nextInt())

  val in = new DataInputStream(socket.getInputStream)
  val out = new DataOutputStream(socket.getOutputStream)

  sendRename(name)

  new Thread(){
    override def run(): Unit = {
      try {
        while (true) {
          val b = in.readByte()
          b match {
            case 0 => id = in.readInt()
            case 1 => destroyRoom()
            case 2 => newRoom()
            case 3 => joinRoom()
            case 4 => leaveRoom()
            case 5 => roomData()
            case 6 => otherJoinRoom()
          }
        }
      } catch {
        case e: Throwable => close()
      }
    }
  }.start()

  def destroyRoom(): Unit = {
    val roomId = in.readInt()
  }

  def newRoom(): Unit = {
    val roomId = in.readInt()
    val name = in.readUTF()
  }

  def joinRoom(): Unit = {
    val roomId = in.readInt()
    val roommateCount = in.readInt()
    for (i <- 0 until roommateCount) {
      val id = in.readInt()
      val name = in.readUTF()
    }
  }

  def leaveRoom(): Unit = {
    val id = in.readInt()
  }

  def roomData(): Unit = {
    val length = in.readInt()
    val buffer = new Array[Byte](length)
    in.readFully(buffer)
  }

  def otherJoinRoom(): Unit = {
    val id = in.readInt()
    val name = in.readUTF()
  }

  def sendExit(): Unit = {
    this.synchronized {
      out.writeByte(0)
    }
  }

  def sendRename(name: String): Unit = {
    this.synchronized {
      out.writeByte(1)
      out.writeUTF(name)
    }
  }

  def sendNewRoom(): Unit = {
    this.synchronized {
      out.writeByte(2)
    }
  }

  def sendJoinRoom(roomId: Int): Unit = {
    this.synchronized {
      out.writeByte(3)
      out.writeByte(roomId)
    }
  }

  def sendLeaveRoom(): Unit = {
    this.synchronized {
      out.writeByte(4)
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
    try {
      socket.close()
    } catch {
      case e: Throwable =>
    }
  }
}
