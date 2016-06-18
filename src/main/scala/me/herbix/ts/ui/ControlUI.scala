package me.herbix.ts.ui

import java.awt.image.BufferedImage
import java.awt.{RenderingHints, Graphics2D, Graphics}
import java.awt.event.{ActionEvent, ActionListener}
import javax.swing.{JButton, JPanel}

import me.herbix.ts.logic.{Faction, OperationInputChooseFaction, State, Game}
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

  setLayout(null)

  for (button <- buttons) {
    add(button)
    button.setVisible(false)
    button.addActionListener(this)
  }

  updateState()

  def updateState(): Unit = {
    game.stateStack.top match {
      case State.start => chooseFactionUI()
      case State.waitOther => waitOtherUI()
      case _ => waitOtherUI()
    }
    repaint()
  }

  def resetText() = for (i <- text.indices) text(i) = ""

  def chooseFactionUI(): Unit = {
    resetText()
    text(3) = Lang.chooseFaction
    buttons(0).setVisible(true)
    buttons(0).setText(Lang.US)
    buttons(0).setLocation(20, 120)
    buttons(0).setSize(70, 30)
    buttons(1).setVisible(true)
    buttons(1).setText(Lang.USSR)
    buttons(1).setLocation(110, 120)
    buttons(1).setSize(70, 30)
    buttons(2).setVisible(false)
  }

  def waitOtherUI(): Unit = {
    resetText()
    text(3) = Lang.waitingForOpposite
    buttons.foreach(_.setVisible(false))
  }

  override def paint(g: Graphics): Unit = {
    super.paint(g)
    g.asInstanceOf[Graphics2D].setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON)
    g.setFont(Resource.textFont)
    for (i <- text.indices) {
      g.drawString(text(i), 20, i * 20 + 10)
    }

    if (img != null) {
      val l = 10
      val t = 45
      val cardWidth = 100
      g.drawImage(img, l, t, cardWidth, img.getHeight * cardWidth / img.getWidth, null)
      g.drawRoundRect(l, t, cardWidth, img.getHeight * cardWidth / img.getWidth, 5, 5)
    }
  }

  override def actionPerformed(e: ActionEvent): Unit = {
    val id = buttonMap(e.getSource)
    game.stateStack.top match {
      case State.start =>
        game.sendNextState(new OperationInputChooseFaction(
          game.playerId,
          if (id == 0) Faction.US else Faction.USSR
        ))
      case _ =>
    }
  }
}
