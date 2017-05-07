package me.herbix.ts.client

import java.awt.event.{ActionEvent, ActionListener}
import java.awt.{Dimension, BorderLayout}
import javax.swing._

import me.herbix.ts.logic.GameVariant
import me.herbix.ts.logic.GameVariant.GameVariant

/**
  * Created by Chaofan on 2016/7/4.
  */
object NewRoomDialog extends JDialog {

  var isDone = false

  setTitle("新建房间")
  setModal(true)
  setResizable(false)

  setLayout(new BorderLayout)

  val panel = new JPanel
  panel.setLayout(null)
  panel.setPreferredSize(new Dimension(400, 320))
  add(panel)

  panel.add(label("游戏变体", 50, 30, 50, 20))
  panel.add(label("苏联让点", 50, 70, 50, 20))
  panel.add(label("平局胜者", 50, 110, 50, 20))
  panel.add(label("可选牌", 50, 150, 50, 20))
  panel.add(label("扩展牌1", 50, 190, 50, 20))
  panel.add(label("扩展牌2", 50, 230, 50, 20))

  val done = new JButton("完成")
  done.setLocation(150, 270)
  done.setSize(100, 30)
  panel.add(done)

  val variant = new JComboBox[GameVariantDelegate]()
  variant.addItem(new GameVariantDelegate(GameVariant.Standard))
  variant.addItem(new GameVariantDelegate(GameVariant.LateWar))
  variant.addItem(new GameVariantDelegate(GameVariant.ChineseCivilWar))
  variant.addItem(new GameVariantDelegate(GameVariant.TurnZero))
  variant.setLocation(125, 30)
  variant.setSize(100, 20)
  variant.setFocusable(false)
  panel.add(variant)

  val slider = new JSlider(-9, 9, 0)
  slider.setLocation(120, 60)
  slider.setSize(230, 40)
  slider.setFocusable(false)
  slider.setPaintTicks(true)
  slider.setMajorTickSpacing(3)
  slider.setMinorTickSpacing(1)
  slider.setPaintLabels(true)
  panel.add(slider)

  val us = new JRadioButton("美国")
  us.setLocation(120, 110)
  us.setSize(50, 20)
  us.setSelected(true)
  panel.add(us)

  val ussr = new JRadioButton("苏联")
  ussr.setLocation(220, 110)
  ussr.setSize(50, 20)
  panel.add(ussr)

  val group = new ButtonGroup
  group.add(us)
  group.add(ussr)

  val optional = new JCheckBox()
  optional.setLocation(120, 150)
  optional.setSize(50, 20)
  optional.setSelected(true)
  panel.add(optional)

  val promo1 = new JCheckBox()
  promo1.setLocation(120, 190)
  promo1.setSize(50, 20)
  promo1.setSelected(true)
  panel.add(promo1)

  val promo2 = new JCheckBox()
  promo2.setLocation(120, 230)
  promo2.setSize(50, 20)
  promo2.setSelected(true)
  panel.add(promo2)

  pack()

  setLocationRelativeTo(getOwner)

  done.addActionListener(new ActionListener {
    override def actionPerformed(e: ActionEvent): Unit = {
      isDone = true
      NewRoomDialog.super.setVisible(false)
    }
  })

  override def setVisible(b: Boolean): Unit = {
    isDone = false
    super.setVisible(b)
  }

  def label(text: String, x: Int, y: Int, w: Int, h: Int): JLabel = {
    val r = new JLabel(text)
    r.setLocation(x, y)
    r.setSize(w, h)
    r
  }

  class GameVariantDelegate(val gameVariant: GameVariant) {
    override def toString = gameVariant match {
      case GameVariant.Standard => "标准"
      case GameVariant.LateWar => "冷战后期"
      case GameVariant.ChineseCivilWar => "中国内战"
      case GameVariant.TurnZero => "第零回合"
    }
  }
}
