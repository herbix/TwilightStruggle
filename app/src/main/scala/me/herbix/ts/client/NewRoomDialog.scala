// Copyright (C) 2017 Chaofan

package me.herbix.ts.client

import java.awt.event.{ActionEvent, ActionListener}
import java.awt.{BorderLayout, Dimension}
import javax.swing._

import me.herbix.ts.logic.GameVariant
import me.herbix.ts.logic.GameVariant.GameVariant
import me.herbix.ts.util.Lang

/**
  * Created by Chaofan on 2016/7/4.
  */
object NewRoomDialog extends JDialog {

  var isDone = false

  setTitle(Lang.newRoom)
  setModal(true)
  setResizable(false)

  setLayout(new BorderLayout)

  val panel = new JPanel
  panel.setLayout(null)
  panel.setPreferredSize(new Dimension(400, 320))
  add(panel)

  panel.add(label(Lang.gameVariant, 40, 30, 150, 20))
  panel.add(label(Lang.extraInfluence, 40, 70, 150, 20))
  panel.add(label(Lang.drawGameWinner, 40, 110, 150, 20))
  panel.add(label(Lang.optionalCards, 40, 150, 150, 20))
  panel.add(label(s"${Lang.promoCards}1", 40, 190, 150, 20))
  panel.add(label(s"${Lang.promoCards}2", 40, 230, 150, 20))

  val done = new JButton(Lang.done)
  done.setLocation(150, 270)
  done.setSize(100, 30)
  panel.add(done)

  val variant = new JComboBox[GameVariantDelegate]()
  variant.addItem(new GameVariantDelegate(GameVariant.Standard))
  variant.addItem(new GameVariantDelegate(GameVariant.LateWar))
  variant.addItem(new GameVariantDelegate(GameVariant.ChineseCivilWar))
  variant.addItem(new GameVariantDelegate(GameVariant.TurnZero))
  variant.setLocation(155, 30)
  variant.setSize(100, 20)
  variant.setFocusable(false)
  panel.add(variant)

  val slider = new JSlider(-9, 9, 0)
  slider.setLocation(150, 60)
  slider.setSize(210, 40)
  slider.setFocusable(false)
  slider.setPaintTicks(true)
  slider.setMajorTickSpacing(3)
  slider.setMinorTickSpacing(1)
  slider.setPaintLabels(true)
  panel.add(slider)

  val us = new JRadioButton(Lang.US)
  us.setLocation(150, 110)
  us.setSize(80, 20)
  us.setSelected(true)
  panel.add(us)

  val ussr = new JRadioButton(Lang.USSR)
  ussr.setLocation(250, 110)
  ussr.setSize(80, 20)
  panel.add(ussr)

  val group = new ButtonGroup
  group.add(us)
  group.add(ussr)

  val optional = new JCheckBox()
  optional.setLocation(150, 150)
  optional.setSize(50, 20)
  optional.setSelected(true)
  panel.add(optional)

  val promo1 = new JCheckBox()
  promo1.setLocation(150, 190)
  promo1.setSize(50, 20)
  promo1.setSelected(true)
  panel.add(promo1)

  val promo2 = new JCheckBox()
  promo2.setLocation(150, 230)
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
      case GameVariant.Standard => Lang.standard
      case GameVariant.LateWar => Lang.lateWar
      case GameVariant.ChineseCivilWar => Lang.chineseCivilWar
      case GameVariant.TurnZero => Lang.turnZero
    }
  }
}
