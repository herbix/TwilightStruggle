package me.herbix.ts.ui

import java.awt.{RenderingHints, Graphics2D, Graphics}
import java.awt.event.{ActionEvent, ActionListener}
import javax.swing.{JButton, JPanel}

import me.herbix.ts.logic.{Faction, OperationInputChooseFaction, State, Game}
import me.herbix.ts.util.Resource

/**
  * Created by Chaofan on 2016/6/17.
  */
class ControlUI(val game: Game) extends JPanel with ActionListener {

  var text = ""
  var buttons = Array(new JButton(), new JButton(), new JButton())
  val buttonMap: Map[AnyRef, Int] = buttons.indices.map(i => (buttons(i), i)).toMap

  setLayout(null)

  for (button <- buttons) {
    add(button)
    button.setVisible(false)
    button.addActionListener(this)
    button.setFont(Resource.textFont)
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

  def chooseFactionUI(): Unit = {
    text = "选择你的阵营："
    buttons(0).setVisible(true)
    buttons(0).setText("美国")
    buttons(0).setLocation(20, 120)
    buttons(0).setSize(70, 50)
    buttons(1).setVisible(true)
    buttons(1).setText("苏联")
    buttons(1).setLocation(110, 120)
    buttons(1).setSize(70, 50)
    buttons(2).setVisible(false)
  }

  def waitOtherUI(): Unit = {
    text = "等待对方行动……"
    buttons.foreach(_.setVisible(false))
  }

  override def paint(g: Graphics): Unit = {
    super.paint(g)
    g.asInstanceOf[Graphics2D].setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON)
    g.setFont(Resource.textFont)
    g.drawString(text, 20, 60)
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
