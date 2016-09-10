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
  private val exit = new JButton("退出")

  singlePlayer.setSize(150, 45)
  singlePlayer.setLocation(75, 180)

  multiPlayer.setSize(150, 45)
  multiPlayer.setLocation(75, 250)

  exit.setSize(150, 45)
  exit.setLocation(75, 320)

  panel.add(singlePlayer)
  panel.add(multiPlayer)
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

  addWindowListener(new WindowAdapter {
    override def windowClosed(e: WindowEvent): Unit = {
      ModeSelector.synchronized {
        ModeSelector.notifyAll()
      }
    }
  })
}
