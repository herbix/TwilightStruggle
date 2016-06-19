package me.herbix.ts.ui

import java.awt.image.BufferedImage
import java.awt.{RenderingHints, Graphics2D, Graphics}
import java.awt.event.{ActionEvent, ActionListener}
import javax.swing.table.DefaultTableModel
import javax.swing.{JScrollPane, JTable, JButton, JPanel}

import me.herbix.ts.logic._
import me.herbix.ts.util.{Lang, Resource}

/**
  * Created by Chaofan on 2016/6/17.
  */
class ControlUI(val game: Game) extends JPanel with ActionListener {

  game.stateUpdateListeners :+= (() => updateState())

  var img: BufferedImage = null
  val text = Array("", "", "", "")
  var buttons = Array(new JButton(), new JButton(), new JButton())
  val buttonMap: Map[AnyRef, Int] = buttons.indices.map(i => (buttons(i), i)).toMap

  val influenceTableModel = new DefaultTableModel()
  val influenceTable = new JTable(influenceTableModel)
  val influenceTableOuter = new JScrollPane(influenceTable)

  var operationListeners: List[Operation => Unit] = List()

  setLayout(null)

  for (button <- buttons) {
    add(button)
    button.setVisible(false)
    button.addActionListener(this)
  }

  influenceTableModel.addColumn("国家")
  influenceTableModel.addColumn("影响力")
  influenceTable.setRowHeight(25)
  influenceTableOuter.setLocation(0, 40)
  influenceTableOuter.setSize(200, 120)
  influenceTableOuter.setBorder(null)
  influenceTableOuter.setVisible(false)
  add(influenceTableOuter)

  updateState()

  def updateState(): Unit = {
    game.stateStack.top match {
      case State.start => chooseFactionUI()
      case State.waitOther => waitOtherUI()
      case State.putStartUSSR =>
        if (game.currentPlayer == Faction.USSR) {
          putInfluenceUI(6, "请在东欧放置%s点影响力", true, _.forall(_._1.regions.contains(Region.EastEurope)))
        } else {
          waitOtherUI()
        }
      case _ => waitOtherUI()
    }
    repaint()
  }

  override def paint(g: Graphics): Unit = {
    super.paint(g)

    g.asInstanceOf[Graphics2D].setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON)
    g.setFont(Resource.textFont)
    val fm = g.getFontMetrics

    for (i <- text.indices) {
      val str = text(i)
      val w = fm.stringWidth(str)
      g.drawString(text(i), (getWidth - w) / 2, i * 20 + 28)
    }

    if (img != null) {
      val l = 10
      val t = 45
      val cardWidth = 100
      g.drawImage(img, l, t, cardWidth, img.getHeight * cardWidth / img.getWidth, null)
      g.drawRoundRect(l, t, cardWidth, img.getHeight * cardWidth / img.getWidth, 5, 5)
    }
  }

  def resetText() = for (i <- text.indices) text(i) = ""
  def resetButtons(): Unit = buttons.foreach(_.setVisible(false))
  def resetAll() = {
    resetText()
    resetButtons()
    influenceTableOuter.setVisible(false)
  }

  def showButton(buttonId: Int, buttonText: String, x: Int, y: Int, w: Int, h: Int): Unit = {
    buttons(buttonId).setVisible(true)
    buttons(buttonId).setText(buttonText)
    buttons(buttonId).setLocation(x, y)
    buttons(buttonId).setSize(w, h)
  }

  def chooseFactionUI(): Unit = {
    resetAll()
    text(2) = Lang.chooseFaction
    showButton(0, Lang.US, 20, 120, 70, 30)
    showButton(1, Lang.USSR, 110, 120, 70, 30)
  }

  def waitOtherUI(): Unit = {
    resetAll()
    text(2) = Lang.waitingForOpposite
  }

  def putInfluenceUI(point: Int, tip: String, ignoreControl: Boolean, valid: Map[Country, Int] => Boolean) = {
    resetAll()
    text(0) = String.format(tip, point.toString)
    influenceTableOuter.setVisible(true)
    showButton(0, "完成", 10, 165, 60, 30)
    showButton(1, "取消", 70, 165, 60, 30)
    showButton(2, "重置", 130, 165, 60, 30)
    influenceTableModel.setNumRows(0)
  }

  override def actionPerformed(e: ActionEvent): Unit = {
    val id = buttonMap(e.getSource)
    val op = game.stateStack.top match {
      case State.start =>
        new OperationChooseFaction(
          game.playerId,
          if (id == 0) Faction.US else Faction.USSR
        )
      case _ => null
    }
    operationListeners.foreach(_(op))
  }
}
