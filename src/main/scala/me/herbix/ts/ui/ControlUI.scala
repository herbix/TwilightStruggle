package me.herbix.ts.ui

import java.awt.{CardLayout, RenderingHints, Graphics2D, Graphics}
import java.awt.event.{MouseEvent, MouseMotionListener, ActionEvent, ActionListener}
import javax.swing.table.DefaultTableModel
import javax.swing._

import me.herbix.ts.logic.Faction.Faction
import me.herbix.ts.logic
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
    val SelectCard = Value
    val SelectCardAndAction = Value
  }

  import UIType._

  var uiType = Waiting

  val uiChooseFaction = new ControlSubUIChooseFaction(this)
  val uiWaiting = new ControlSubUIText(this, Array("", "", Lang.waitingForOpposite))
  val uiInfluence = new ControlSubUIModifyInfluence(this)
  val uiSelectCard = new ControlSubUISelectCard(this)
  val uiSelectCardAndAction = new ControlSubUISelectCardAndAction(this)

  var operationListeners: List[Operation => Unit] = List()

  setLayout(new CardLayout)
  add(uiChooseFaction, ChooseFaction.toString)
  add(uiWaiting, Waiting.toString)
  add(uiInfluence, Influence.toString)
  add(uiSelectCard, SelectCard.toString)
  add(uiSelectCardAndAction, SelectCardAndAction.toString)

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
        if (game.playerFaction == Faction.USSR) {
          addInfluenceUI(6, Lang.putEastEurope, true, true, game.playerFaction, _.forall(_._1.regions.contains(Region.EastEurope)))
        } else {
          waitOtherUI()
        }
      case State.putStartUS =>
        if (game.playerFaction == Faction.US) {
          addInfluenceUI(7, Lang.putWestEurope, true, true, game.playerFaction, _.forall(_._1.regions.contains(Region.WestEurope)))
        } else {
          waitOtherUI()
        }
      case State.putStartUSExtra =>
        if (game.playerFaction == Faction.US) {
          addInfluenceUI(1, Lang.putExtra, true, true, game.playerFaction, _.forall(_._1.influence(Faction.US) > 0))
        } else {
          waitOtherUI()
        }
      case State.selectHeadlineCard => selectCardUI(Lang.selectHeadline)
      case State.selectCardAndAction =>
        if (game.playerFaction == game.phasingPlayer) {
          selectCardAndActionUI()
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

  def selectCardUI(tip: String) = {
    showSubUI(SelectCard)
    uiSelectCard.text(0) = tip
    uiSelectCard.resetCard()
  }

  def selectCardAndActionUI() = {
    showSubUI(SelectCardAndAction)
    uiSelectCardAndAction.resetCard()
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

abstract class ControlSubUICard(parent: ControlUI, array: Array[String]) extends ControlSubUIText(parent, array) {
  var img = Resource.card(0)
  var card = Cards.fromId(0)

  var cardHoverListeners: List[Card => Unit] = List()

  def resetCard() = setCard(Cards.fromId(0))

  def setCard(card: Card): Unit = {
    img = Resource.card(card.id)
    this.card = card
    updateCard()
    repaint()
  }

  def updateCard(): Unit

  override def paint(g: Graphics): Unit = {
    super.paint(g)

    g.asInstanceOf[Graphics2D].setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BILINEAR)

    if (img != null) {
      val l = 10
      val t = 45
      val cardWidth = 100
      g.drawImage(img, l, t, cardWidth, img.getHeight * cardWidth / img.getWidth, null)
      g.drawRoundRect(l, t, cardWidth, img.getHeight * cardWidth / img.getWidth, 5, 5)
    }
  }

  addMouseMotionListener(new MouseMotionListener {
    override def mouseMoved(e: MouseEvent): Unit = {
      val x = e.getX
      val y = e.getY
      if (x >= 10 && x < 110 && y >= 45 && y < 185) {
        cardHoverListeners.foreach(_(card))
      }
    }
    override def mouseDragged(e: MouseEvent): Unit = {}
  })
}

class ControlSubUIChooseFaction(parent: ControlUI) extends
  ControlSubUIText(parent, Array("", "", Lang.selectFaction)) {

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

  val buttonDone = addButton(Lang.done, 10, 165, 60, 30)
  val buttonCancel = addButton(Lang.cancel, 70, 165, 60, 30)
  val buttonReset = addButton(Lang.reset, 130, 165, 60, 30)

  val tableModel = new DefaultTableModel()
  val table = new JTable(tableModel)
  val tableOuter = new JScrollPane(table)
  var tip = "%s"
  var point = 0
  var ignoreControl = false
  var isAdd = true
  var targetFaction = parent.game.playerFaction
  var validCheck: Map[Country, Int] => Boolean = null
  var pendingInfluenceChange: mutable.Map[Country, Int] = null

  var updateListeners: List[() => Unit] = List()

  tableModel.addColumn(Lang.country)
  tableModel.addColumn(Lang.influence)
  table.setRowHeight(25)
  table.setSelectionMode(ListSelectionModel.SINGLE_SELECTION)
  tableOuter.setLocation(0, 40)
  tableOuter.setSize(200, 120)
  tableOuter.setBorder(null)
  add(tableOuter)

  def updatePendingInfluenceChange(): Unit = {
    tableModel.setRowCount(0)
    for ((country, modifyValue) <- pendingInfluenceChange) {
      val influence = country.influence(parent.game.playerFaction)
      tableModel.addRow(Array[Object](new CountryDelegate(country), f"$influence -> ${influence + modifyValue}"))
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
      val influence = country.influence(parent.game.playerFaction)
      val c =
        if (ignoreControl)
          modifyValue
        else {
          val influenceOpposite = country.influence(Faction.getOpposite(parent.game.playerFaction))
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
          val country = tableModel.getValueAt(selectedRow, 0).asInstanceOf[CountryDelegate].country
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

  class CountryDelegate(val country: Country) {
    override def toString = Lang.countryNames(country.name)
  }
}

class ControlSubUISelectCard(parent: ControlUI) extends ControlSubUICard(parent, Array("")) {

  val buttonDone = addButton(Lang.done, 120, 100, 70, 30)

  override def updateCard(): Unit = {
    buttonDone.setEnabled(card.id != 0)
  }

  override def actionPerformed(e: ActionEvent): Unit = {
    val op = new OperationSelectCard(parent.game.playerId, parent.game.playerFaction, card)
    parent.operationListeners.foreach(_(op))
  }

}

class ControlSubUISelectCardAndAction(parent: ControlUI) extends ControlSubUICard(parent, Array(Lang.selectCardAndAction)) {

  val buttonSpace = addButton(Lang.space, 120, 50, 70, 30)
  val buttonEvent = addButton(Lang.event, 120, 100, 70, 30)
  val buttonOperation = addButton(Lang.operation, 120, 150, 70, 30)

  override def updateCard(): Unit = {
    val b = card.id != 0
    buttonSpace.setEnabled(b && parent.game.canSpace(parent.game.playerFaction))
    buttonEvent.setEnabled(b)
    buttonOperation.setEnabled(b)
    if (card.faction == Faction.getOpposite(parent.game.playerFaction)) {
      buttonEvent.setText(Lang.eventFirst)
      buttonOperation.setText(Lang.operationFirst)
    } else {
      buttonEvent.setText(Lang.event)
      buttonOperation.setText(Lang.operation)
    }
  }

  override def actionPerformed(e: ActionEvent): Unit = {
    val op = e.getSource match {
      case this.buttonSpace =>
        new OperationSelectCardAndAction(parent.game.playerId, parent.game.playerFaction, card, logic.Action.Space)
      case this.buttonEvent =>
        new OperationSelectCardAndAction(parent.game.playerId, parent.game.playerFaction, card, logic.Action.Event)
      case this.buttonOperation =>
        new OperationSelectCardAndAction(parent.game.playerId, parent.game.playerFaction, card, logic.Action.Operation)
      case _ =>
        null
    }
    parent.operationListeners.foreach(_(op))
  }
}

