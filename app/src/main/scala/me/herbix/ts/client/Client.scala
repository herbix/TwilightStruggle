// Copyright (C) 2017 Chaofan

package me.herbix.ts.client

import java.awt.Font
import java.util
import javax.swing.UIManager
import javax.swing.plaf.FontUIResource

import me.herbix.ts.util.{Lang, Resource}

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

  def InitGlobalFont(font: Font): Unit = {
    val globalFont = new FontUIResource(font)
    val keys = UIManager.getDefaults.keys()
    while (keys.hasMoreElements) {
      val key = keys.nextElement()
      if (UIManager.get(key).isInstanceOf[FontUIResource]) {
        UIManager.put(key, globalFont)
      }
    }
  }

  def main(args: Array[String]): Unit = {
    UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName)
    InitGlobalFont(new Font(Lang.songti, 0, 12))

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
