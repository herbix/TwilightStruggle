package me.herbix.ts.client

import java.awt.event.{ActionEvent, ActionListener}
import java.awt.{BorderLayout, Dimension}
import java.util.Properties
import javax.swing._
import javax.swing.table.DefaultTableModel

import io.netty.bootstrap.Bootstrap
import io.netty.channel.nio.NioEventLoopGroup
import io.netty.channel.socket.SocketChannel
import io.netty.channel.socket.nio.NioSocketChannel
import io.netty.channel.{ChannelInitializer, ChannelOption}
import me.herbix.ts.client.NewRoomDialog.GameVariantDelegate
import me.herbix.ts.logic.{Faction, GameVariant}
import me.herbix.ts.netcommon.NetCodec
import me.herbix.ts.util.{Config, Lang}

import scala.collection.mutable

/**
  * Created by Chaofan on 2016/7/3.
  */
object MultiplePlayerFrame extends JFrame {

  val gameVersion = "ts-" + getGameVersion

  var netHandler: NetHandlerClient = null

  var extraInfluence = 0
  var hasOptional = false
  var hasPromo1 = false
  var hasPromo2 = false
  var drawWinner = Faction.Neutral
  var gameVariant = GameVariant.Standard

  val roomCreatorMap = mutable.Map.empty[Int, Int]

  setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)

  setLayout(new BorderLayout)

  val tableModel = new DefaultTableModel
  val table = new JTable(tableModel)
  val tableOut = new JScrollPane(table)

  add(tableOut)

  tableModel.addColumn(Lang.roomId)
  tableModel.addColumn(Lang.roomName)
  tableModel.addColumn(Lang.gameVersion)

  table.setRowHeight(25)
  table.getColumnModel.getColumn(0).setPreferredWidth(50)
  table.getColumnModel.getColumn(1).setPreferredWidth(400)
  table.getColumnModel.getColumn(2).setPreferredWidth(100)
  table.setSelectionMode(ListSelectionModel.SINGLE_SELECTION)

  tableOut.setPreferredSize(new Dimension(600, 400))

  val panelBottom = new JPanel
  add(panelBottom, BorderLayout.SOUTH)

  val newRoom = new JButton(Lang.`new`)
  newRoom.setPreferredSize(new Dimension(100, 30))
  val joinRoom = new JButton(Lang.join)
  joinRoom.setPreferredSize(new Dimension(100, 30))

  panelBottom.add(newRoom)
  panelBottom.add(joinRoom)

  pack()

  setLocationRelativeTo(getOwner)

  setTitle(s"${Lang.twilightStruggle}[$gameVersion] - ${Lang.connecting}")
  newRoom.setEnabled(false)
  joinRoom.setEnabled(false)

  new Thread() {
    override def run(): Unit = {
      val workGroup = new NioEventLoopGroup()

      try {
        val bootstrap = new Bootstrap()
        bootstrap.group(workGroup)
          .channel(classOf[NioSocketChannel])
          .option(ChannelOption.SO_KEEPALIVE.asInstanceOf[ChannelOption[Any]], true)
          .handler(new ChannelInitializer[SocketChannel] {
            override def initChannel(ch: SocketChannel): Unit = {
              netHandler = new NetHandlerClient
              ch.pipeline().addLast(new NetCodec, netHandler)
            }
          })

        val channelFuture = bootstrap.connect(Config.host, Config.port).sync()

        SwingUtilities.invokeLater(new Runnable {
          override def run(): Unit = {
            setTitle(s"${Lang.twilightStruggle}[$gameVersion] - " + netHandler.name)
            newRoom.setEnabled(true)
            joinRoom.setEnabled(true)
          }
        })

        channelFuture.channel().closeFuture().sync()
      } catch {
        case e: Throwable =>
          SwingUtilities.invokeLater(new Runnable {
            override def run(): Unit = {
              setTitle(s"${Lang.twilightStruggle}[$gameVersion] - ${Lang.connectFailed}")
            }
          })
      } finally {
        workGroup.shutdownGracefully()
      }
    }
  }.start()

  newRoom.addActionListener(new ActionListener {
    override def actionPerformed(e: ActionEvent): Unit = {
      NewRoomDialog.setVisible(true)
      if (NewRoomDialog.isDone) {
        netHandler.sendNewRoom()
        extraInfluence = NewRoomDialog.slider.getValue
        drawWinner = if (NewRoomDialog.us.isSelected) Faction.US else Faction.USSR
        hasOptional = NewRoomDialog.optional.isSelected
        hasPromo1 = NewRoomDialog.promo1.isSelected
        hasPromo2 = NewRoomDialog.promo2.isSelected
        gameVariant = NewRoomDialog.variant.getSelectedItem.asInstanceOf[GameVariantDelegate].gameVariant
        showInfo()
      }
    }
  })

  def showInfo(): Unit = {
    RoomDialog.info.setText("<html><body>" +
      s"${Lang.gameVariant}: ${new GameVariantDelegate(gameVariant)}<br/>" +
      s"${Lang.extraInfluence}: $extraInfluence<br/>" +
      s"${Lang.drawGameWinner}: ${Lang.getFactionName(drawWinner)}<br/>" +
      s"${Lang.extraCards}: ${if (hasOptional) s"${Lang.optional} " else ""}" +
      s"${if (hasPromo1) s"${Lang.promo}1 " else ""}${if (hasPromo2) s"${Lang.promo}2 " else ""}<br/>" +
      "</body></html>"
    )
  }

  joinRoom.addActionListener(new ActionListener {
    override def actionPerformed(e: ActionEvent): Unit = {
      val n = table.getSelectedRow
      if (n >= 0) {
        val version = tableModel.getValueAt(n, 2).toString
        try {
          if (version.substring(0, version.lastIndexOf('.')) == gameVersion.substring(0, gameVersion.lastIndexOf('.'))) {
            netHandler.sendJoinRoom(tableModel.getValueAt(n, 0).asInstanceOf[Int])
          } else {
            throw new Exception("version check exception")
          }
        } catch {
          case e: Throwable =>
            JOptionPane.showMessageDialog(MultiplePlayerFrame, Lang.versionNotMatch, Lang.twilightStruggle, JOptionPane.ERROR_MESSAGE)
        }
      }
    }
  })

  def getGameVersion: String = {
    val p = new Properties()
    p.load(getClass.getResourceAsStream("/version.property"))
    p.getProperty("version")
  }

}
