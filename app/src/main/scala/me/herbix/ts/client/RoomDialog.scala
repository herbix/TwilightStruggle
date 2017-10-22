// Copyright (C) 2017 Chaofan

package me.herbix.ts.client

import java.awt.event._
import java.awt.{BorderLayout, Dimension}
import javax.swing._
import javax.swing.table.DefaultTableModel

import me.herbix.ts.ui.GameUI
import me.herbix.ts.util.Lang

/**
  * Created by Chaofan on 2016/7/4.
  */
object RoomDialog extends JDialog {

  var gameUI: GameUI = null

  setTitle(Lang.room)
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
  tableOut.setSize(400, 180)
  panel.add(tableOut)

  tableModel.addColumn(Lang.playerId)
  tableModel.addColumn(Lang.playerName)

  table.setRowHeight(25)
  table.getColumnModel.getColumn(0).setPreferredWidth(50)
  table.getColumnModel.getColumn(1).setPreferredWidth(300)

  val info = new JLabel("")
  info.setLocation(20, 180)
  info.setSize(260, 90)
  panel.add(info)

  val start = new JButton(Lang.start)
  start.setLocation(300, 210)
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
        MultiplePlayerFrame.netHandler.sendLeaveRoom()
      } catch {
        case e: Throwable =>
      }
    }
  })

  start.addActionListener(new ActionListener {
    override def actionPerformed(e: ActionEvent): Unit = {
      try {
        MultiplePlayerFrame.netHandler.roomSendStart()
      } catch {
        case e: Throwable =>
      }
    }
  })

}
