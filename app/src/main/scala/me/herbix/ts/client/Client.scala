package me.herbix.ts.client

import javax.swing.UIManager

import me.herbix.ts.util.Resource

/**
  * Created by Chaofan on 2016/9/10.
  */
object Client {

  new Thread() {
    override def run(): Unit = {
      val time = System.nanoTime()
      Resource.getClass
      println("Loading Resource: " + (System.nanoTime() - time) / 1e9 + "s")
    }
  }.start()

  def main(args: Array[String]): Unit = {
    UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName)

    ModeSelector.synchronized {
      ModeSelector.setVisible(true)
      while (ModeSelector.isVisible) {
        ModeSelector.wait(1000)
      }
    }

    ModeSelector.result match {
      case ModeSelector.SinglePlayer => SinglePlayerFrame.setVisible(true)
      case ModeSelector.MultiPlayer => MultiplePlayerFrame.setVisible(true)
      case _ => System.exit(0)
    }
  }

}
