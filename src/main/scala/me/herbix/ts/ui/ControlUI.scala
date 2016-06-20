package me.herbix.ts.ui

import java.awt.image.BufferedImage
import java.awt.{CardLayout, RenderingHints, Graphics2D, Graphics}
import java.awt.event.{ActionEvent, ActionListener}
import javax.swing.table.DefaultTableModel
import javax.swing._

import me.herbix.ts.logic.Faction.Faction
import me.herbix.ts.logic._
import me.herbix.ts.util.{Lang, Resource}

import scala.collection.mutable

/**
  * Created by Chaofan on 2016/6/17.
  */
class ControlUI(val game: Game) extends JPanel {

  game.stateUpdateListeners :+= (() => updateState())

  object UIType extends Enumeration {
    type UIType = Value
    val ChooseFaction, Waiting = Value
    val Influence = Value
  }

  import UIType._

  var uiType = Waiting

  val uiChooseFaction = new ControlSubUIChooseFaction(this)
  val uiWaiting = new ControlSubUIText(this, Array("", "", Lang.waitingForOpposite))
  val uiInfluence = new ControlSubUIModifyInfluence(this)

  var operationListeners: List[Operation => Unit] = List()

  setLayout(new CardLayout)
  add(uiChooseFaction, ChooseFaction.toString)
  add(uiWaiting, Waiting.toString)
  add(uiInfluence, Influence.toString)

  updateState()

  def showSubUI(uiType: UIType) = {
    this.uiType = uiType
    getLayout.asInstanceOf[CardLayout].show(this, uiType.toString)
  }

  def updateState(): Unit = {
    game.stateStack.top match {
      case State.start => chooseFactionUI()
      case State.waitOther => waitOtherUI()
      case State.putStartUSSR =>
        if (game.currentPlayer == Faction.USSR) {
          addInfluenceUI(6, Lang.putEastEurope, true, true, game.currentPlayer, _.forall(_._1.regions.contains(Region.EastEurope)))
        } else {
          waitOtherUI()
        }
      case State.putStartUS =>
        if (game.currentPlayer == Faction.US) {
          addInfluenceUI(7, Lang.putWestEurope, true, true, game.currentPlayer, _.forall(_._1.regions.contains(Region.WestEurope)))
        } else {
          waitOtherUI()
        }
      case State.putStartUSExtra =>
        if (game.currentPlayer == Faction.US) {
          addInfluenceUI(1, Lang.putExtra, true, true, game.currentPlayer, _.forall(_._1.influence(Faction.US) > 0))
        } else {
          waitOtherUI()
        }
      case _ => waitOtherUI()
    }
    repaint()
  }

  def chooseFactionUI(): Unit = showSubUI(ChooseFaction)
  def waitOtherUI(): Unit = showSubUI(Waiting)

  def addInfluenceUI(point: Int, tip: String, isAdd: Boolean, ignoreControl: Boolean, targetFaction: Faction, valid: Map[Country, Int] => Boolean) = {
    showSubUI(Influence)
    uiInfluence.tip = tip
    uiInfluence.point = point
    uiInfluence.isAdd = isAdd
    uiInfluence.ignoreControl = ignoreControl
    uiInfluence.targetFaction = targetFaction
    uiInfluence.validCheck = valid
    uiInfluence.text(0) = String.format(tip, point.toString)
    uiInfluence.updatePendingInfluenceChange()
  }

}

abstract class ControlSubUIBase(val control: ControlUI) extends JPanel with ActionListener {
  setLayout(null)

  protected def addButton(buttonText: String, x: Int, y: Int, w: Int, h: Int): JButton = {
    val button = new JButton()
    button.setText(buttonText)
    button.setLocation(x, y)
    button.setSize(w, h)
    button.addActionListener(this)
    add(button)
    button
  }

  override def actionPerformed(e: ActionEvent): Unit = throw new NotImplementedError
}

class ControlSubUIText(parent: ControlUI, val text: Array[String]) extends ControlSubUIBase(parent) {

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
  }
}

class ControlSubUIImage(parent: ControlUI, array: Array[String]) extends ControlSubUIText(parent, array) {
  var img: BufferedImage = null

  override def paint(g: Graphics): Unit = {
    super.paint(g)

    if (img != null) {
      val l = 10
      val t = 45
      val cardWidth = 100
      g.drawImage(img, l, t, cardWidth, img.getHeight * cardWidth / img.getWidth, null)
      g.drawRoundRect(l, t, cardWidth, img.getHeight * cardWidth / img.getWidth, 5, 5)
    }
  }
}

class ControlSubUIChooseFaction(parent: ControlUI) extends
  ControlSubUIText(parent, Array("", "", Lang.chooseFaction)) {

  val usButton = addButton(Lang.US, 20, 120, 70, 30)
  val ussrButton  = addButton(Lang.USSR, 110, 120, 70, 30)

  override def actionPerformed(e: ActionEvent): Unit = {
    val faction = e.getSource match {
      case this.usButton => Faction.US
      case this.ussrButton => Faction.USSR
    }
    val op = new OperationChooseFaction(parent.game.playerId, faction)
    parent.operationListeners.foreach(_(op))
  }
}

class ControlSubUIModifyInfluence(parent: ControlUI) extends
  ControlSubUIText(parent, Array("")) {

  val buttonDone = addButton("完成", 10, 165, 60, 30)
  val buttonCancel = addButton("取消", 70, 165, 60, 30)
  val buttonReset = addButton("重置", 130, 165, 60, 30)

  val tableModel = new DefaultTableModel()
  val table = new JTable(tableModel)
  val tableOuter = new JScrollPane(table)
  var tip = "%s"
  var point = 0
  var ignoreControl = false
  var isAdd = true
  var targetFaction = parent.game.currentPlayer
  var validCheck: Map[Country, Int] => Boolean = null
  var pendingInfluenceChange: mutable.Map[Country, Int] = null

  var updateListeners: List[() => Unit] = List()

  tableModel.addColumn("国家")
  tableModel.addColumn("影响力")
  table.setRowHeight(25)
  table.setSelectionMode(ListSelectionModel.SINGLE_SELECTION)
  tableOuter.setLocation(0, 40)
  tableOuter.setSize(200, 120)
  tableOuter.setBorder(null)
  add(tableOuter)

  def updatePendingInfluenceChange(): Unit = {
    tableModel.setRowCount(0)
    for ((country, modifyValue) <- pendingInfluenceChange) {
      val influence = country.influence(parent.game.currentPlayer)
      tableModel.addRow(Array[Object](country, f"$influence -> ${influence + modifyValue}"))
    }
    val rest = point - calculateInfluenceCost()
    text(0) = String.format(tip, rest.toString)
    buttonDone.setEnabled(rest == 0)
    if (tableModel.getRowCount > 0) {
      table.setRowSelectionInterval(0, 0)
    }
    updateListeners.foreach(_())
    repaint()
  }

  def checkInfluence(): Boolean = {
    calculateInfluenceCost() <= point && validCheck(pendingInfluenceChange.toMap) &&
      (isAdd || pendingInfluenceChange.forall(e => e._1.influence(targetFaction) >= e._2))
  }

  def calculateInfluenceCost(): Int = {
    var cost = 0
    for ((country, modifyValue) <- pendingInfluenceChange) {
      val influence = country.influence(parent.game.currentPlayer)
      val c =
        if (ignoreControl)
          modifyValue
        else {
          val influenceOpposite = country.influence(Faction.getOpposite(parent.game.currentPlayer))
          if (influenceOpposite - influence >= country.stability) {
            modifyValue + Math.min(modifyValue, influenceOpposite - influence - country.stability + 1)
          } else {
            modifyValue
          }
        }
      cost += c
    }
    cost
  }

  def addInfluence(country: Country, value: Int): Unit = {
    if (pendingInfluenceChange.contains(country)) {
      pendingInfluenceChange(country) += value
    } else {
      pendingInfluenceChange += country -> value
    }
    if (checkInfluence()) {
      updatePendingInfluenceChange()
    } else {
      if (pendingInfluenceChange(country) == value) {
        pendingInfluenceChange -= country
      } else {
        pendingInfluenceChange(country) -= value
      }
    }
  }

  override def actionPerformed(e: ActionEvent): Unit = {
    e.getSource match {
      case this.buttonReset =>
        pendingInfluenceChange.clear()
        updatePendingInfluenceChange()
      case this.buttonCancel =>
        val selectedRow = table.getSelectedRow
        if (selectedRow >= 0) {
          val country = tableModel.getValueAt(selectedRow, 0).asInstanceOf[Country]
          pendingInfluenceChange(country) -= 1
          if (pendingInfluenceChange(country) == 0) {
            pendingInfluenceChange -= country
          }
          updatePendingInfluenceChange()
          if (tableModel.getRowCount > selectedRow) {
            table.setRowSelectionInterval(selectedRow, selectedRow)
          }
        }
      case this.buttonDone =>
        val op = new OperationModifyInfluence(
          parent.game.playerId,
          targetFaction,
          isAdd,
          pendingInfluenceChange.toMap
        )
        pendingInfluenceChange.clear()
        parent.operationListeners.foreach(_(op))
    }
  }
}

