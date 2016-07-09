package me.herbix.ts.client

import java.awt.event.{WindowEvent, WindowAdapter}
import java.io.{DataOutputStream, DataInputStream}
import java.net.Socket
import javax.swing.{WindowConstants, SwingUtilities}

import me.herbix.ts.logic.{Faction, GameVariant, Operation, Game}
import me.herbix.ts.ui.GameUI
import me.herbix.ts.util.Serializer._

import scala.collection.mutable
import scala.util.Random

/**
  * Created by Chaofan on 2016/7/3.
  */
class NetHandlerClient(socket: Socket) {

  var id = 0
  var name: String = "TS-" + Integer.toHexString(Random.nextInt())

  var isRoomCreator = false

  val in = new DataInputStream(socket.getInputStream)
  val out = new DataOutputStream(socket.getOutputStream)

  val roomInReal: RoomDataInputStream = new RoomDataInputStream()
  val roomIn = new DataInputStream(roomInReal)
  val roomOut = new DataOutputStream(new RoomDataOutputStream(this))

  val random = new Random()
  var seed = 0l

  sendVersion(ClientFrame.gameVersion)
  sendRename(name)

  new Thread() {
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
        case e: Throwable =>
          e.printStackTrace()
          close()
      }
    }
  }.start()

  new Thread() {
    override def run(): Unit = {
      try {
        while (true) {
          val b = roomIn.readByte()
          b match {
            case 0 => roomProperty()
            case 1 => roomStart()
            case 2 => roomOperation()
          }
        }
      } catch {
        case e: Throwable =>
          e.printStackTrace()
          close()
      }
    }
  }.start()

  def destroyRoom(): Unit = {
    val id = in.readInt()
    ClientFrame.roomCreatorMap -= id
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
    val version = in.readUTF()
    ClientFrame.roomCreatorMap += roomId -> creatorId
    println(s"newRoom $roomId $creatorId $name")
    SwingUtilities.invokeLater(new Runnable {
      override def run(): Unit = {
        val model = ClientFrame.tableModel
        model.addRow(Array[Object](Integer.valueOf(roomId), name, version))
      }
    })
  }

  def joinRoom(): Unit = {
    val roomId = in.readInt()
    val roommateCount = in.readInt()
    val info = mutable.Set.empty[(Int, String)]
    for (i <- 0 until roommateCount) {
      val id = in.readInt()
      val name = in.readUTF()
      info += ((id, name))
    }
    isRoomCreator = ClientFrame.roomCreatorMap(roomId) == id
    RoomDialog.start.setEnabled(isRoomCreator)
    println(s"joinRoom $roomId")
    SwingUtilities.invokeLater(new Runnable {
      override def run(): Unit = {
        RoomDialog.tableModel.setRowCount(0)
        for ((id, name) <- info) {
          RoomDialog.tableModel.addRow(Array[Object](Integer.valueOf(id), name))
        }
        RoomDialog.setVisible(true)
      }
    })
  }

  def leaveRoom(): Unit = {
    val id = in.readInt()
    println(s"leaveRoom $id")
    SwingUtilities.invokeLater(new Runnable {
      override def run(): Unit = {
        if (id == NetHandlerClient.this.id) {
          RoomDialog.setVisible(false)
          if (RoomDialog.gameUI != null) {
            RoomDialog.gameUI.setVisible(false)
            RoomDialog.gameUI = null
          }
        } else {
          val r = (0 until RoomDialog.tableModel.getRowCount).find(RoomDialog.tableModel.getValueAt(_, 0).asInstanceOf[Int] == id)
          for (i <- r) {
            RoomDialog.tableModel.removeRow(i)
          }
        }
      }
    })
  }

  def roomData(): Unit = {
    val length = in.readInt()
    val buffer = new Array[Byte](length)
    in.readFully(buffer)
    roomInReal.fill(buffer)
  }

  def otherJoinRoom(): Unit = {
    val id = in.readInt()
    val name = in.readUTF()
    println(s"otherJoinRoom $id")
    if (isRoomCreator) {
      roomSendProperty()
    }
    SwingUtilities.invokeLater(new Runnable {
      override def run(): Unit = {
        RoomDialog.tableModel.addRow(Array[Object](Integer.valueOf(id), name))
      }
    })
  }

  def sendExit(): Unit = {
    println("send exit")
    this.synchronized {
      out.writeByte(0)
    }
  }

  def sendRename(name: String): Unit = {
    println("send rename " + name)
    this.synchronized {
      out.writeByte(1)
      out.writeUTF(name)
    }
  }

  def sendNewRoom(): Unit = {
    println("send newRoom")
    this.synchronized {
      out.writeByte(2)
    }
  }

  def sendJoinRoom(roomId: Int): Unit = {
    println("send joinRoom " + roomId)
    this.synchronized {
      out.writeByte(3)
      out.writeInt(roomId)
    }
  }

  def sendLeaveRoom(): Unit = {
    println("send leaveRoom")
    this.synchronized {
      out.writeByte(4)
    }
  }

  def sendRoomData(buffer: Array[Byte]): Unit = {
    this.synchronized {
      out.writeByte(5)
      out.writeInt(buffer.length)
      out.write(buffer)
    }
  }

  def sendVersion(version: String): Unit = {
    println("send version " + version)
    this.synchronized {
      out.writeByte(6)
      out.writeUTF(version)
    }
  }

  def roomProperty(): Unit = {
    println("roomProperty")
    ClientFrame.extraInfluence = roomIn.readInt()
    ClientFrame.drawWinner = Faction(roomIn.readInt())
    ClientFrame.hasOptional = roomIn.readBoolean()
    ClientFrame.gameVariant = GameVariant(roomIn.readInt())
    ClientFrame.showInfo()
  }

  def roomStart(): Unit = {
    println("roomStart")
    seed = roomIn.readLong()
    showGame()
  }

  def roomOperation(): Unit = {
    val game = if (RoomDialog.gameUI != null) RoomDialog.gameUI.game else null
    val input = readOperation(roomIn, game)
    println("roomOperation " + input.toString)
    SwingUtilities.invokeLater(new Runnable {
      override def run(): Unit = {
        game.nextState(input)
      }
    })
  }

  def roomSendProperty(): Unit = {
    println("roomSendProperty")
    roomOut.writeByte(0)
    roomOut.writeInt(ClientFrame.extraInfluence)
    roomOut.writeInt(ClientFrame.drawWinner.id)
    roomOut.writeBoolean(ClientFrame.hasOptional)
    roomOut.writeInt(ClientFrame.gameVariant.id)
    roomOut.flush()
  }

  def roomSendStart(): Unit = {
    println("roomSendStart")
    roomOut.writeByte(1)
    seed = random.nextLong()
    roomOut.writeLong(seed)
    roomOut.flush()
    showGame()
  }

  def roomSendOperation(input: Operation): Unit = {
    println("roomSendOperation " + input.toString)
    roomOut.writeByte(2)
    input.writeToStream(roomOut)
    roomOut.flush()
  }

  def showGame(): Unit = {
    val gameUI = new GameUI(id)
    RoomDialog.gameUI = gameUI
    RoomDialog.setVisible(false)
    gameUI.game.extraInfluence = ClientFrame.extraInfluence
    gameUI.game.optionalCards = ClientFrame.hasOptional
    gameUI.game.drawGameWinner = ClientFrame.drawWinner
    gameUI.game.gameVariant = ClientFrame.gameVariant
    gameUI.game.anotherGame = new RemoteGame(this)
    gameUI.game.setRandomSeed(seed)
    gameUI.setVisible(true)
    gameUI.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE)
    gameUI.addWindowListener(new WindowAdapter {
      override def windowClosed(e: WindowEvent): Unit = {
        RoomDialog.gameUI = null
        RoomDialog.setVisible(true)
      }
    })
  }

  def close(): Unit = {
    roomIn.close()
    try {
      socket.close()
    } catch {
      case e: Throwable =>
    }
  }
}
