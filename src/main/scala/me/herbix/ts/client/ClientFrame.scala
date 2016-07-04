package me.herbix.ts.client

import java.awt.event.{ActionEvent, ActionListener}
import java.awt.{Dimension, ScrollPane, BorderLayout}
import java.net.Socket
import javax.swing.table.DefaultTableModel
import javax.swing._

/**
  * Created by Chaofan on 2016/7/3.
  */
object ClientFrame extends JFrame {

  var netHandler: NetHandlerClient = null

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
        val socket = new Socket("localhost", 23981)
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
      }
    }
  })

  def main(args: Array[String]): Unit = {
    setVisible(true)
  }

}
