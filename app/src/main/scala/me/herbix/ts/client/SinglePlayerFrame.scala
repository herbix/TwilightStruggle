package me.herbix.ts.client

import java.awt.Dimension
import javax.swing.{WindowConstants, JPanel, JLabel, JFrame}

import me.herbix.ts.client.MultiplePlayerFrame._
import me.herbix.ts.client.NewRoomDialog.GameVariantDelegate
import me.herbix.ts.logic.{GameVariant, Faction}
import me.herbix.ts.util.Lang

/**
  * Created by Chaofan on 2016/9/10.
  */
object SinglePlayerFrame extends JFrame {

  var extraInfluence = 0
  var hasOptional = false
  var drawWinner = Faction.Neutral
  var gameVariant = GameVariant.Standard

  setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)

  val info = new JLabel()
  showInfo()

  setTitle("冷战热斗")

  val panel = new JPanel()
  panel.setPreferredSize(new Dimension(400, 300))



  add(panel)

  pack()
  setLocationRelativeTo(getOwner)

  setResizable(false)

  def showInfo(): Unit = {
    info.setText("<html><body>" +
      s"游戏变体：${new GameVariantDelegate(gameVariant)}<br/>" +
      s"苏联让点：$extraInfluence<br/>" +
      s"平局胜者：${Lang.getFactionName(drawWinner)}<br/>" +
      s"可选牌：　${if (hasOptional) "有" else "无"}<br/>" +
      "</body></html>"
    )
  }
}
