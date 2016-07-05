package me.herbix.ts.client

import java.awt.event.{ActionEvent, ActionListener}
import java.awt.{Dimension, ScrollPane, BorderLayout}
import java.net.Socket
import javax.swing.table.DefaultTableModel
import javax.swing._

import me.herbix.ts.logic.Faction
import me.herbix.ts.util.Lang

import scala.collection.mutable

/**
  * Created by Chaofan on 2016/7/3.
  */
object ClientFrame extends JFrame {

  var netHandler: NetHandlerClient = null

  var extraInfluence = 0
  var hasOptional = false
  var drawWinner = Faction.Neutral

  val roomCreatorMap = mutable.Map.empty[Int, Int]

  UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName)
  setLayout(new BorderLayout)

  setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)

  setLayout(new BorderLayout)

  val tableModel = new DefaultTableModel
  val table = new JTable(tableModel)
  val tableOut = new JScrollPane(table)

  add(tableOut)

  tableModel.addColumn("房间号")
  tableModel.addColumn("房间名")

  table.setRowHeight(25)
  table.getColumnModel.getColumn(0).setPreferredWidth(50)
  table.getColumnModel.getColumn(1).setPreferredWidth(500)
  table.setSelectionMode(ListSelectionModel.SINGLE_SELECTION)

  tableOut.setPreferredSize(new Dimension(600, 400))

  val panelBottom = new JPanel
  add(panelBottom, BorderLayout.SOUTH)

  val newRoom = new JButton("新建")
  newRoom.setPreferredSize(new Dimension(100, 30))
  val joinRoom = new JButton("加入")
  joinRoom.setPreferredSize(new Dimension(100, 30))

  panelBottom.add(newRoom)
  panelBottom.add(joinRoom)

  pack()

  setLocationRelativeTo(getOwner)

  setTitle("冷战热斗 - 正在连接...")
  newRoom.setEnabled(false)
  joinRoom.setEnabled(false)

  new Thread() {
    override def run(): Unit = {
      try {
        val socket = new Socket("ts.herbix.me", 23981)
        netHandler = new NetHandlerClient(socket)
        SwingUtilities.invokeLater(new Runnable {
          override def run(): Unit = {
            setTitle("冷战热斗 - " + netHandler.name)
            newRoom.setEnabled(true)
            joinRoom.setEnabled(true)
          }
        })
      } catch {
        case e: Throwable =>
          SwingUtilities.invokeLater(new Runnable {
            override def run(): Unit = {
              setTitle("冷战热斗 - 连接失败")
            }
          })
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
        showInfo()
      }
    }
  })

  def showInfo(): Unit = {
    RoomDialog.info.setText("<html><body>" +
      s"苏联让点：$extraInfluence<br/>" +
      s"平局胜者：${Lang.getFactionName(drawWinner)}<br/>" +
      s"可选牌：  ${if (hasOptional) "有" else "无"}<br/>" +
      "</body></html>"
    )
  }

  joinRoom.addActionListener(new ActionListener {
    override def actionPerformed(e: ActionEvent): Unit = {
      val n = table.getSelectedRow
      if (n >= 0) {
        netHandler.sendJoinRoom(tableModel.getValueAt(n, 0).asInstanceOf[Int])
      }
    }
  })

  def main(args: Array[String]): Unit = {
    setVisible(true)
  }

}