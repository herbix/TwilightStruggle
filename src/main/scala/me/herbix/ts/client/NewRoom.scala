package me.herbix.ts.client

import java.awt.{Dimension, BorderLayout}
import javax.swing._

/**
  * Created by Chaofan on 2016/7/4.
  */
object NewRoom extends JDialog {

  var isDone = false

  setTitle("新建房间")
  setModal(true)
  setResizable(false)

  setLayout(new BorderLayout)

  val panel = new JPanel
  panel.setLayout(null)
  panel.setPreferredSize(new Dimension(400, 200))
  add(panel)

  panel.add(label("苏联让点", 50, 30, 50, 20))
  panel.add(label("平局胜者", 50, 70, 50, 20))
  panel.add(label("可选牌", 50, 110, 50, 20))

  val done = new JButton("完成")
  done.setLocation(150, 150)
  done.setSize(100, 30)
  panel.add(done)

  val slider = new JSlider(-9, 9, 0)
  slider.setLocation(120, 20)
  slider.setSize(230, 40)
  slider.setFocusable(false)
  slider.setPaintTicks(true)
  slider.setMajorTickSpacing(3)
  slider.setMinorTickSpacing(1)
  slider.setPaintLabels(true)
  panel.add(slider)

  val us = new JRadioButton("美国")
  us.setLocation(120, 70)
  us.setSize(50, 20)
  us.setSelected(true)
  panel.add(us)

  val ussr = new JRadioButton("苏联")
  ussr.setLocation(220, 70)
  ussr.setSize(50, 20)
  panel.add(ussr)

  val group = new ButtonGroup
  group.add(us)
  group.add(ussr)

  val optional = new JCheckBox()
  optional.setLocation(120, 110)
  optional.setSize(50, 20)
  optional.setSelected(true)
  panel.add(optional)

  pack()

  setLocationRelativeTo(getOwner)

  override def setVisible(b: Boolean): Unit = {
    super.setVisible(b)
    isDone = false
  }

  def label(text: String, x: Int, y: Int, w: Int, h: Int): JLabel = {
    val r = new JLabel(text)
    r.setLocation(x, y)
    r.setSize(w, h)
    r
  }
}
