package me.herbix.ts.client

import java.awt.event._
import java.awt.{Dimension, BorderLayout}
import javax.swing._
import javax.swing.table.DefaultTableModel

import me.herbix.ts.client.NewRoomDialog._
import me.herbix.ts.ui.GameUI

/**
  * Created by Chaofan on 2016/7/4.
  */
object RoomDialog extends JDialog {

  var gameUI: GameUI = null

  setTitle("房间")
  setModal(true)
  setResizable(false)

  setLayout(new BorderLayout)

  val panel = new JPanel
  panel.setLayout(null)
  panel.setPreferredSize(new Dimension(400, 270))
  add(panel)

  val tableModel = new DefaultTableModel
  val table = new JTable(tableModel)
  val tableOut = new JScrollPane(table)

  tableOut.setLocation(0, 0)
  tableOut.setSize(400, 200)
  panel.add(tableOut)

  tableModel.addColumn("玩家号")
  tableModel.addColumn("玩家名")

  table.setRowHeight(25)
  table.getColumnModel.getColumn(0).setPreferredWidth(50)
  table.getColumnModel.getColumn(1).setPreferredWidth(300)

  val info = new JLabel("")
  info.setLocation(20, 210)
  info.setSize(260, 50)
  panel.add(info)

  val start = new JButton("开始")
  start.setLocation(300, 220)
  start.setSize(80, 30)
  panel.add(start)

  pack()

  setLocationRelativeTo(getOwner)

  addWindowListener(new WindowAdapter {
    override def windowClosing(e: WindowEvent): Unit = {
      if (gameUI != null) {
        return
      }
      try {
        ClientFrame.netHandler.sendLeaveRoom()
      } catch {
        case e: Throwable =>
      }
    }
  })

  start.addActionListener(new ActionListener {
    override def actionPerformed(e: ActionEvent): Unit = {
      try {
        ClientFrame.netHandler.roomSendStart()
      } catch {
        case e: Throwable =>
      }
    }
  })

}
