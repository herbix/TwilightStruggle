package me.herbix.ts.client

import java.awt.Dimension
import java.awt.event._
import javax.swing._

/**
  * Created by Chaofan on 2016/9/10.
  */
object ModeSelector extends JFrame {

  val SinglePlayer = "SinglePlayer"
  val MultiPlayer = "MultiPlayer"

  var result: String = null

  setTitle("冷战热斗")

  private val panel = new JPanel()

  panel.setPreferredSize(new Dimension(300, 400))
  panel.setLayout(null)

  add(panel)

  private val singlePlayer = new JButton("单人游戏")
  private val multiPlayer = new JButton("多人游戏")
  private val help = new JButton("帮助")
  private val exit = new JButton("退出")

  singlePlayer.setSize(120, 40)
  singlePlayer.setLocation(90, 180)

  multiPlayer.setSize(120, 40)
  multiPlayer.setLocation(90, 230)

  help.setSize(120, 40)
  help.setLocation(90, 280)

  exit.setSize(120, 40)
  exit.setLocation(90, 330)

  panel.add(singlePlayer)
  panel.add(multiPlayer)
  panel.add(help)
  panel.add(exit)

  pack()
  setLocationRelativeTo(getOwner)

  setResizable(false)

  singlePlayer.addActionListener(new ActionListener {
    override def actionPerformed(e: ActionEvent): Unit = {
      result = SinglePlayer
      ModeSelector.synchronized {
        setVisible(false)
        ModeSelector.notifyAll()
      }
    }
  })

  multiPlayer.addActionListener(new ActionListener {
    override def actionPerformed(e: ActionEvent): Unit = {
      result = MultiPlayer
      ModeSelector.synchronized {
        setVisible(false)
        ModeSelector.notifyAll()
      }
    }
  })

  exit.addActionListener(new ActionListener {
    override def actionPerformed(e: ActionEvent): Unit = {
      ModeSelector.synchronized {
        setVisible(false)
        ModeSelector.notifyAll()
      }
    }
  })

  help.addActionListener(new ActionListener {
    override def actionPerformed(e: ActionEvent): Unit = {
      HelpDialog.setVisible(true)
    }
  })

  addWindowListener(new WindowAdapter {
    override def windowClosed(e: WindowEvent): Unit = {
      ModeSelector.synchronized {
        ModeSelector.notifyAll()
      }
    }
  })
}
