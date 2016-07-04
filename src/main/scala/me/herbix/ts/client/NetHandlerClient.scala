package me.herbix.ts.client

import java.io.{DataOutputStream, DataInputStream}
import java.net.Socket
import javax.swing.SwingUtilities

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
    val id = in.readInt()
    println(s"destroyRoom $id")
    SwingUtilities.invokeLater(new Runnable {
      override def run(): Unit = {
        val model = ClientFrame.tableModel
        val r = (0 until model.getRowCount) find { i =>
          val roomId = model.getValueAt(i, 0).asInstanceOf[Int]
          roomId == id
        }
        for (i <- r) {
          model.removeRow(i)
        }
      }
    })
  }

  def newRoom(): Unit = {
    val roomId = in.readInt()
    val creatorId = in.readInt()
    val name = in.readUTF()
    println(s"newRoom $roomId $creatorId $name")
    SwingUtilities.invokeLater(new Runnable {
      override def run(): Unit = {
        val model = ClientFrame.tableModel
        model.addRow(Array[Object](Integer.valueOf(roomId), name))
      }
    })
  }

  def joinRoom(): Unit = {
    val roomId = in.readInt()
    val roommateCount = in.readInt()
    for (i <- 0 until roommateCount) {
      val id = in.readInt()
      val name = in.readUTF()
    }
    println(s"joinRoom $roomId")
  }

  def leaveRoom(): Unit = {
    val id = in.readInt()
    println(s"leaveRoom $id")
  }

  def roomData(): Unit = {
    val length = in.readInt()
    val buffer = new Array[Byte](length)
    in.readFully(buffer)
  }

  def otherJoinRoom(): Unit = {
    val id = in.readInt()
    val name = in.readUTF()
    println(s"otherJoinRoom $id")
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
