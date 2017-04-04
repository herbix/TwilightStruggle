package me.herbix.ts.client

import java.awt.event.{WindowAdapter, WindowEvent}
import java.io.{DataInputStream, DataOutputStream}
import javax.swing.{SwingUtilities, WindowConstants}

import io.netty.channel.{ChannelHandlerContext, SimpleChannelInboundHandler}
import me.herbix.ts.logic._
import me.herbix.ts.netcommon._
import me.herbix.ts.ui.GameUI
import me.herbix.ts.util.Serializer._

import scala.util.Random

/**
  * Created by Chaofan on 2016/7/3.
  */
class NetHandlerClient extends SimpleChannelInboundHandler[Packet] {

  var id = 0
  var name: String = System.getProperty("user.name", "TS-" + Integer.toHexString(Random.nextInt()))

  var isRoomCreator = false

  val roomInReal: RoomDataInputStream = new RoomDataInputStream()
  val roomIn = new DataInputStream(roomInReal)
  val roomOut = new DataOutputStream(new RoomDataOutputStream(this))

  val random = new Random()
  var seed = 0l

  var ctx: ChannelHandlerContext = null

  override def channelActive(ctx: ChannelHandlerContext): Unit = {
    this.ctx = ctx
    sendVersion(MultiplePlayerFrame.gameVersion)
    sendRename(name)
  }

  override def channelRead0(ctx: ChannelHandlerContext, msg: Packet): Unit = {
    msg match {
      case p: SPacketId => id = p.id
      case p: SPacketNewRoom => newRoom(p)
      case p: SPacketJoinRoom => joinRoom(p)
      case p: SPacketOtherJoinRoom => otherJoinRoom(p)
      case p: SPacketLeaveRoom => leaveRoom(p)
      case p: SPacketDestroyRoom => destroyRoom(p)
      case p: PacketRoomData => roomData(p)
      case p: Packet => println(s"$id $p")
    }
  }

  override def exceptionCaught(ctx: ChannelHandlerContext, cause: Throwable): Unit = {
    println(s"$id $cause")
    close()
  }

  new Thread() {
    override def run(): Unit = {
      try {
        while (true) {
          val b = roomIn.readByte()
          b match {
            case 0 => roomProperty()
            case 1 => roomStart()
            case 2 => roomOperation()
            case 3 => roomRollBack()
          }
        }
      } catch {
        case e: Throwable =>
          e.printStackTrace()
          close()
      }
    }
  }.start()

  def destroyRoom(packet: SPacketDestroyRoom): Unit = {
    val id = packet.id
    MultiplePlayerFrame.roomCreatorMap -= id
    println(s"destroyRoom $id")
    SwingUtilities.invokeLater(new Runnable {
      override def run(): Unit = {
        val model = MultiplePlayerFrame.tableModel
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

  def newRoom(packet: SPacketNewRoom): Unit = {
    val roomId = packet.id
    val creatorId = packet.creator
    val name = packet.name
    val version = packet.version
    MultiplePlayerFrame.roomCreatorMap += roomId -> creatorId
    println(s"newRoom $roomId $creatorId $name")
    SwingUtilities.invokeLater(new Runnable {
      override def run(): Unit = {
        val model = MultiplePlayerFrame.tableModel
        model.addRow(Array[Object](Integer.valueOf(roomId), name, version))
      }
    })
  }

  def joinRoom(packet: SPacketJoinRoom): Unit = {
    val roomId = packet.id
    val info = packet.members
    isRoomCreator = MultiplePlayerFrame.roomCreatorMap(roomId) == id
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

  def leaveRoom(packet: SPacketLeaveRoom): Unit = {
    val id = packet.id
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

  def roomData(packet: PacketRoomData): Unit = {
    roomInReal.fill(packet.bytes)
  }

  def otherJoinRoom(packet: SPacketOtherJoinRoom): Unit = {
    val id = packet.id
    val name = packet.name
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
    ctx.writeAndFlush(new CPacketExit())
  }

  def sendRename(name: String): Unit = {
    println("send rename " + name)
    ctx.writeAndFlush(new CPacketRename(name))
  }

  def sendNewRoom(): Unit = {
    println("send newRoom")
    ctx.writeAndFlush(new CPacketNewRoom())
  }

  def sendJoinRoom(roomId: Int): Unit = {
    println("send joinRoom " + roomId)
    ctx.writeAndFlush(new CPacketJoinRoom(roomId))
  }

  def sendLeaveRoom(): Unit = {
    println("send leaveRoom")
    ctx.writeAndFlush(new CPacketLeaveRoom())
  }

  def sendRoomData(buffer: Array[Byte]): Unit = {
    ctx.writeAndFlush(new PacketRoomData(buffer))
  }

  def sendVersion(version: String): Unit = {
    println("send version " + version)
    ctx.writeAndFlush(new CPacketVersion(version))
  }

  def sendEnableJoin(enable: Boolean): Unit = {
    println("send enableJoin " + enable)
    ctx.writeAndFlush(new CPacketEnableJoin(enable))
  }

  def roomProperty(): Unit = {
    println("roomProperty")
    MultiplePlayerFrame.extraInfluence = roomIn.readInt()
    MultiplePlayerFrame.drawWinner = Faction(roomIn.readInt())
    MultiplePlayerFrame.hasOptional = roomIn.readBoolean()
    MultiplePlayerFrame.gameVariant = GameVariant(roomIn.readInt())
    MultiplePlayerFrame.showInfo()
  }

  def roomStart(): Unit = {
    println("roomStart")
    seed = roomIn.readLong()
    showGame()
  }

  def roomOperation(): Unit = {
    implicit val game = if (RoomDialog.gameUI != null) RoomDialog.gameUI.game else null
    val input = readOperation(roomIn)
    println("roomOperation " + input.toString)
    SwingUtilities.invokeLater(new Runnable {
      override def run(): Unit = {
        game.nextState(input)
      }
    })
  }

  def roomRollBack(): Unit = {
    val game = if (RoomDialog.gameUI != null) RoomDialog.gameUI.game else null
    val id = roomIn.readInt()
    println("roomRollBack " + id)
    SwingUtilities.invokeLater(new Runnable {
      override def run(): Unit = {
        game.rollBackBeforeHistory(id)
      }
    })
  }

  def roomSendProperty(): Unit = {
    println("roomSendProperty")
    roomOut.writeByte(0)
    roomOut.writeInt(MultiplePlayerFrame.extraInfluence)
    roomOut.writeInt(MultiplePlayerFrame.drawWinner.id)
    roomOut.writeBoolean(MultiplePlayerFrame.hasOptional)
    roomOut.writeInt(MultiplePlayerFrame.gameVariant.id)
    roomOut.flush()
  }

  def roomSendStart(): Unit = {
    println("roomSendStart")
    roomOut.writeByte(1)
    seed = random.nextLong()
    roomOut.writeLong(seed)
    roomOut.flush()
    sendEnableJoin(false)
    showGame()
  }

  def roomSendOperation(input: Operation): Unit = {
    println("roomSendOperation " + input.toString)
    roomOut.writeByte(2)
    input.writeToStream(roomOut)
    roomOut.flush()
  }

  def roomSendRollBack(historyId: Int): Unit = {
    println("roomSendRollBack " + historyId)
    roomOut.writeByte(3)
    roomOut.writeInt(historyId)
    roomOut.flush()
  }

  def showGame(): Unit = {
    val gameUI = new GameUI(id)
    gameUI.init(GameFactory.createGameByVariant(MultiplePlayerFrame.gameVariant))
    RoomDialog.gameUI = gameUI
    RoomDialog.setVisible(false)
    gameUI.game.extraInfluence = MultiplePlayerFrame.extraInfluence
    gameUI.game.optionalCards = MultiplePlayerFrame.hasOptional
    gameUI.game.drawGameWinner = MultiplePlayerFrame.drawWinner
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
    sendExit()
    roomIn.close()
    ctx.close()
  }

}
