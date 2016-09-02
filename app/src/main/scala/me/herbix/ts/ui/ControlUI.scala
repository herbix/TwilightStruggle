package me.herbix.ts.ui

import java.awt.event.{ActionEvent, ActionListener, MouseEvent, MouseMotionListener}
import java.awt._
import javax.swing._
import javax.swing.table.DefaultTableModel

import me.herbix.ts.logic
import me.herbix.ts.logic.Faction.Faction
import me.herbix.ts.logic._
import me.herbix.ts.logic.card._
import me.herbix.ts.util.{Lang, Resource}

import scala.collection.mutable
import scala.List

/**
  * Created by Chaofan on 2016/6/17.
  */
class ControlUI(val game: Game) extends JPanel {

  game.stateUpdateListeners :+= (() => updateState())

  object UIType extends Enumeration {
    type UIType = Value
    val ChooseFaction = Value
    val Waiting = Value
    val Spectator = Value
    val Influence = Value
    val SelectCard = Value
    val SelectCardAndAction = Value
    val SelectOperation = Value
    val SelectCountry = Value
    val SelectCardOrCancel = Value
    val YesNo = Value
    val Confirm = Value
    val SelectRegion = Value
    val SelectMultipleCards = Value
    val GameOver = Value
    // Special
    val Summit = Value
    val StopWorry = Value
    val GrainSales = Value
  }

  import UIType._

  var uiType = Waiting
  val uiMap = mutable.Map[UIType, ControlSubUIBase]()

  val uiChooseFaction = new ControlSubUIChooseFaction(this)
  val uiWaiting = new ControlSubUIText(this, Array("", "", Lang.waitingForOpposite))
  val uiSpectator = new ControlSubUIText(this, Array("", "", Lang.spectator))
  val uiInfluence = new ControlSubUIModifyInfluence(this)
  val uiSelectCard = new ControlSubUISelectCard(this)
  val uiSelectCardAndAction = new ControlSubUISelectCardAndAction(this)
  val uiSelectOperation = new ControlSubUISelectOperation(this)
  val uiSelectCountry = new ControlSubUISelectCountry(this)
  val uiSelectCardOrCancel = new ControlSubUISelectCardOrCancel(this)
  val uiYesNo = new ControlSubUIYesNo(this)
  val uiConfirm = new ControlSubUIConfirm(this)
  val uiSelectRegion = new ControlSubUISelectRegion(this)
  val uiSelectMultipleCards = new ControlSubUISelectMultipleCards(this)
  val uiGameOver = new ControlSubUIText(this, Array("", "", Lang.gameOver, "", Lang.winnerIs))
  // Special
  val uiSummit = new ControlSubUISummit(this)
  val uiStopWorry = new ControlSubUIHowILearnStopWorry(this)
  val uiGrainSales = new ControlSubUIGrainSales(this)

  var operationListeners: List[Operation => Unit] = List()
  var uiUpdateListeners: List[() => Unit] = List()

  setLayout(new CardLayout)
  addUI(uiChooseFaction, ChooseFaction)
  addUI(uiWaiting, Waiting)
  addUI(uiSpectator, Spectator)
  addUI(uiInfluence, Influence)
  addUI(uiSelectCard, SelectCard)
  addUI(uiSelectCardAndAction, SelectCardAndAction)
  addUI(uiSelectOperation, SelectOperation)
  addUI(uiSelectCountry, SelectCountry)
  addUI(uiSelectCardOrCancel, SelectCardOrCancel)
  addUI(uiYesNo, YesNo)
  addUI(uiConfirm, Confirm)
  addUI(uiSelectRegion, SelectRegion)
  addUI(uiSelectMultipleCards, SelectMultipleCards)
  addUI(uiGameOver, GameOver)
  // Special
  addUI(uiSummit, Summit)
  addUI(uiStopWorry, StopWorry)
  addUI(uiGrainSales, GrainSales)

  updateState()

  def addUI(subUI: ControlSubUIBase, addedUIType: UIType): Unit = {
    uiMap += addedUIType -> subUI
    add(subUI, addedUIType.toString)
  }

  def showSubUI(showedUIType: UIType) = {
    uiType = showedUIType
    getLayout.asInstanceOf[CardLayout].show(this, showedUIType.toString)
  }

  def updateState(): Unit = {
    if (game.isSpectator) {
      spectatorUI()
      return
    }

    val oldUI = uiMap(uiType)

    val state = game.stateStack.top
    val tip = state match {
      case State.putStartUSSR => Lang.putEastEurope
      case State.putStartUS => Lang.putWestEurope
      case State.putStartExtra | State.noradInfluence => Lang.putExtra
      case State.cardOperationAddInfluence => Lang.operationAddInfluence
      case State.selectHeadlineCard | State.selectHeadlineCard2 => Lang.selectHeadline
      case State.discardHeldCard => Lang.discardHeldCard
      case State.quagmireDiscard => Lang.selectQuagmireDiscard
      case State.quagmirePlayScoringCard => Lang.selectQuagmireScoringCard
      case State.cardOperationRealignment => Lang.operationRealignment
      case State.cardOperationCoup => Lang.operationCoup
      case State.cubaMissileRemove => Lang.cardTips(Card040CubaMissile)(0)
      case State.selectTake8Rounds => Lang.take8rounds
      case State.EventStates(_) =>
        val card = game.currentCard
        val step = card.asInstanceOf[CardMultiStep].getStep(game)
        Lang.cardTips(card)(step-1)
      case _ => ""
    }

    game.getOperationHint match {
      case OperationHint.NOP =>
        if (state == State.end) {
          gameOverUI()
        } else {
          waitOtherUI()
        }
      case OperationHint.CHOOSE_FACTION => chooseFactionUI()
      case OperationHint.SELECT_REGION => selectRegionUI()
      case oh: OperationModifyInfluenceHint =>
        addInfluenceUI(oh.point, tip, oh.isAdd, oh.ignoreControl, oh.mustAllPoints, oh.modifyOp, oh.targetFaction, oh.valid)
      case oh: OperationSelectCardHint =>
        if (oh.canNull) {
          selectCardOrCancelUI(tip)
        } else {
          selectCardUI(tip)
        }
      case oh: OperationSelectCardAndActionHint =>
        selectCardAndActionUI(oh.canSpace, oh.canEvent, oh.canOperation, oh.presetCard)
      case oh: OperationSelectOperationHint =>
        selectOperationUI(oh.canInfluence(game), oh.canRealignment(game), oh.canCoup(game))
      case oh: OperationSelectCountryHint =>
        selectCountryUI(oh.count, oh.countSecondary, tip, oh.mustAllPoints, oh.valid)
      case oh: OperationYesNoHint =>
        if (state == State.cardEventSpecial && game.currentCard == Card067GrainSales) {
          showSubUI(GrainSales)
          uiGrainSales.setCard(game.currentCardData.asInstanceOf[Card])
        } else if (oh.isConfirm) {
          confirmUI(tip)
        } else {
          yesNoUI(tip)
        }
      case oh: OperationIntValueHint =>
        game.currentCard match {
          case Card045Summit => showSubUI(Summit)
          case Card046HowILearnStopWorry => showSubUI(StopWorry)
        }
      case oh: OperationSelectCardsHint =>
        selectMultipleCardsUI(tip);
      case _ => waitOtherUI()
    }

    if (state != State.selectHeadlineCard || oldUI != uiMap(uiType)) {
      oldUI.reset()
    }

    uiUpdateListeners.foreach(_())
    repaint()
  }

  def chooseFactionUI(): Unit = showSubUI(ChooseFaction)
  def waitOtherUI(): Unit = showSubUI(Waiting)
  def spectatorUI(): Unit = showSubUI(Spectator)

  def addInfluenceUI(point: Int, tip: String, isAdd: Boolean, ignoreControl: Boolean,
                     mustAllPoints: Boolean, modifyOp: Boolean, targetFaction: Faction,
                     valid: (Game, Map[Country, Int]) => Boolean) = {
    showSubUI(Influence)
    uiInfluence.tip = tip
    uiInfluence.point = point
    uiInfluence.isAdd = isAdd
    uiInfluence.ignoreControl = ignoreControl
    uiInfluence.mustAllPoints = mustAllPoints
    uiInfluence.modifyOp = modifyOp
    uiInfluence.targetFaction = targetFaction
    uiInfluence.validCheck = valid
    uiInfluence.updatePendingInfluenceChange()
  }

  def selectCardUI(tip: String) = {
    showSubUI(SelectCard)
    uiSelectCard.text(0) = tip
    uiSelectCard.updateCard()
  }

  def selectMultipleCardsUI(tip: String) = {
    showSubUI(SelectMultipleCards)
    uiSelectMultipleCards.text(2) = tip
  }

  def selectCardAndActionUI(canSpace: (Game, Card) => Boolean, canEvent: (Game, Card) => Boolean,
                            canOperation: (Game, Card) => Boolean, card: Card = null) = {
    showSubUI(SelectCardAndAction)
    if (card == null) {
      uiSelectCardAndAction.isLocked = false
      uiSelectCardAndAction.resetCard()
    } else {
      uiSelectCardAndAction.setCard(card)
      uiSelectCardAndAction.isLocked = true
    }
    uiSelectCardAndAction.canSpace = canSpace
    uiSelectCardAndAction.canEvent = canEvent
    uiSelectCardAndAction.canOperation = canOperation
  }

  def selectOperationUI(canInfluence: Boolean, canRealignment: Boolean, canCoup: Boolean) = {
    showSubUI(SelectOperation)
    uiSelectOperation.text(1) = String.format(Lang.operationSelect, game.modifyOp(game.playerFaction, game.currentCard.op).toString)
    uiSelectOperation.influence.setEnabled(canInfluence)
    uiSelectOperation.realignment.setEnabled(canRealignment)
    uiSelectOperation.coup.setEnabled(canCoup)
  }

  def selectCountryUI(point: Int, point2: Int, tip: String, mustAllPoints: Boolean,
                      valid: (Game, Set[Country]) => Boolean): Unit = {
    showSubUI(SelectCountry)
    uiSelectCountry.tip = tip
    uiSelectCountry.point = point
    uiSelectCountry.point2 = point2
    uiSelectCountry.mustAllPoints = mustAllPoints
    uiSelectCountry.validCheck = valid
    uiSelectCountry.updatePendingCountrySelection()
  }

  def selectCardOrCancelUI(tip: String) = {
    showSubUI(SelectCardOrCancel)
    uiSelectCardOrCancel.text(0) = tip
    uiSelectCardOrCancel.resetCard()
  }

  def yesNoUI(tip: String) = {
    showSubUI(YesNo)
    uiYesNo.text(2) = tip
  }

  def confirmUI(tip: String) = {
    showSubUI(Confirm)
    uiConfirm.text(2) = tip
  }

  def selectRegionUI() = {
    showSubUI(SelectRegion)
  }

  def gameOverUI(): Unit = {
    showSubUI(GameOver)
    if (game.operatingPlayer == Faction.Neutral) {
      uiGameOver.text(4) = Lang.drawGame
    } else {
      uiGameOver.text(4) = String.format(Lang.winnerIs, Lang.getFactionName(game.operatingPlayer))
    }
  }

  override def setBackground(color: Color): Unit = {
    super.setBackground(color)
    for (component <- this.getComponents) {
      component match {
        case c: ControlSubUIBase =>
          c.setBackground(color)
        case _ =>
      }
    }
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

  def reset(): Unit
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

  override def reset(): Unit = {}
}

object ControlSubUICard {
  var cardHoverListeners: List[Card => Unit] = List()
}

abstract class ControlSubUICard(parent: ControlUI, array: Array[String]) extends ControlSubUIText(parent, array) {
  var img = Resource.card(0)
  var card = Cards.fromId(0)

  var isLocked = false

  def resetCard() = setCard(Cards.fromId(0))

  def setCard(card: Card): Unit = {
    if (isLocked) return

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
        ControlSubUICard.cardHoverListeners.foreach(_(card))
      }
    }
    override def mouseDragged(e: MouseEvent): Unit = {}
  })

  override def reset(): Unit = resetCard()
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
  ControlSubUIText(parent, Array("")) { // TODO what to do when no avaliable target

  val buttonDone = addButton(Lang.done, 10, 165, 60, 30)
  val buttonCancel = addButton(Lang.remove, 70, 165, 60, 30)
  val buttonReset = addButton(Lang.reset, 130, 165, 60, 30)

  val tableModel = new DefaultTableModel()
  val table = new JTable(tableModel)
  val tableOuter = new JScrollPane(table)
  var tip = "%s"
  var point = 0
  var ignoreControl = false
  var isAdd = true
  var mustAllPoints = true
  var modifyOp = true
  var targetFaction = parent.game.playerFaction
  var validCheck: (Game, Map[Country, Int]) => Boolean = null
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
      val influence = parent.game.influence(country, targetFaction)
      val changedValue = influence + modifyValue * (if (isAdd) 1 else -1)
      tableModel.addRow(Array[Object](new CountryDelegate(country), f"$influence -> $changedValue"))
    }
    val rest = getRest
    text(0) = String.format(tip, rest.toString)
    buttonDone.setEnabled(rest == 0 || !mustAllPoints)
    if (tableModel.getRowCount > 0) {
      table.setRowSelectionInterval(0, 0)
    }
    updateListeners.foreach(_())
    repaint()
  }

  def getRest: Int = {
    getPoint(pendingInfluenceChange) - parent.game.calculateInfluenceCost(pendingInfluenceChange, targetFaction, ignoreControl)
  }

  def getPoint(pendingInfluenceChange: mutable.Map[Country, Int]): Int =
    if (modifyOp) parent.game.modifyOp(parent.game.playerFaction, point, pendingInfluenceChange.keys) else point

  def checkInfluence(pendingInfluenceChange: mutable.Map[Country, Int]): Boolean =
    validCheck(parent.game, pendingInfluenceChange.toMap)

  def addInfluence(country: Country, value: Int): Unit = {
    if (pendingInfluenceChange.contains(country)) {
      pendingInfluenceChange(country) += value
    } else {
      pendingInfluenceChange += country -> value
    }
    if (checkInfluence(pendingInfluenceChange)) {
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

  override def reset(): Unit = {
    pendingInfluenceChange.clear()
    updateListeners.foreach(_())
  }
}

class CountryDelegate(val country: Country) {
  override def toString = Lang.countryNames(country.name)
}

class ControlSubUISelectCard(parent: ControlUI) extends ControlSubUICard(parent, Array("")) {

  val buttonDone = addButton(Lang.done, 120, 100, 70, 30)

  override def updateCard(): Unit = {
    buttonDone.setEnabled(card.id != 0)
  }

  override def actionPerformed(e: ActionEvent): Unit = {
    val op = new OperationSelectCard(parent.game.playerId, parent.game.playerFaction, Some(card))
    parent.operationListeners.foreach(_(op))
    resetCard()
  }

}

class ControlSubUISelectCardAndAction(parent: ControlUI)
  extends ControlSubUICard(parent, Array(Lang.selectCardAndAction)) {

  var canSpace: (Game, Card) => Boolean = (_, _) => true
  var canEvent: (Game, Card) => Boolean = (_, _) => true
  var canOperation: (Game, Card) => Boolean = (_, _) => true

  val buttonSpace = addButton(Lang.space, 120, 50, 70, 30)
  val buttonEvent = addButton(Lang.event, 120, 100, 70, 30)
  val buttonOperation = addButton(Lang.operation, 120, 150, 70, 30)

  override def updateCard(): Unit = {
    val eventEnabled = canEvent(parent.game, card)

    buttonSpace.setEnabled(canSpace(parent.game, card) && card.id != 0)
    buttonEvent.setEnabled(eventEnabled && card.id != 0)
    buttonOperation.setEnabled(canOperation(parent.game, card) && card.id != 0)

    if (card.faction == Faction.getOpposite(parent.game.playerFaction) && eventEnabled) {
      buttonEvent.setText(Lang.eventFirst)
      buttonOperation.setText(Lang.operationFirst)
    } else {
      buttonEvent.setText(Lang.event)
      buttonOperation.setText(Lang.operation)
    }
  }

  override def actionPerformed(e: ActionEvent): Unit = {
    val action = e.getSource match {
      case this.buttonSpace => logic.Action.Space
      case this.buttonEvent => logic.Action.Event
      case this.buttonOperation => logic.Action.Operation
    }
    val op = new OperationSelectCardAndAction(parent.game.playerId, parent.game.playerFaction, card, action)
    parent.operationListeners.foreach(_(op))
  }
}

class ControlSubUISelectOperation(parent: ControlUI) extends ControlSubUIText(parent, Array("", "", "")) {

  val influence = addButton(Lang.influence, 40, 85, 120, 30)
  val realignment = addButton(Lang.realignment, 40, 120, 120, 30)
  val coup = addButton(Lang.coup, 40, 155, 120, 30)

  override def actionPerformed(e: ActionEvent): Unit = {
    val action = e.getSource match {
      case this.influence => logic.Action.Influence
      case this.realignment => logic.Action.Realignment
      case this.coup => logic.Action.Coup
    }
    val op = new OperationSelectOperation(parent.game.playerId, parent.game.playerFaction, action)
    parent.operationListeners.foreach(_(op))
  }

}

class ControlSubUISelectCountry(parent: ControlUI) extends
  ControlSubUIText(parent, Array("")) {

  val buttonDone = addButton(Lang.done, 10, 165, 60, 30)
  val buttonCancel = addButton(Lang.remove, 70, 165, 60, 30)
  val buttonReset = addButton(Lang.reset, 130, 165, 60, 30)

  val tableModel = new DefaultTableModel()
  val table = new JTable(tableModel)
  val tableOuter = new JScrollPane(table)

  var tip = "%s(%s)"
  var point = 0
  var point2 = 0
  var mustAllPoints = true
  var validCheck: (Game, Set[Country]) => Boolean = null

  var pendingCountrySelection: mutable.Set[Country] = null

  var updateListeners: List[() => Unit] = List()

  tableModel.addColumn(Lang.country)
  tableModel.addColumn(Lang.influence)
  table.setRowHeight(25)
  table.setSelectionMode(ListSelectionModel.SINGLE_SELECTION)
  tableOuter.setLocation(0, 40)
  tableOuter.setSize(200, 120)
  tableOuter.setBorder(null)
  add(tableOuter)

  def toggleCountry(country: Country): Unit = {
    if (pendingCountrySelection(country)) {
      pendingCountrySelection -= country
      updatePendingCountrySelection()

    } else if (point == 1 && pendingCountrySelection.nonEmpty) {
      val oldCountry = pendingCountrySelection.head
      pendingCountrySelection.clear()
      pendingCountrySelection += country
      if (checkSelection(pendingCountrySelection)) {
        updatePendingCountrySelection()
      } else {
        pendingCountrySelection.clear()
        pendingCountrySelection += oldCountry
      }

    } else {
      pendingCountrySelection += country
      if (checkSelection(pendingCountrySelection)) {
        updatePendingCountrySelection()
      } else {
        pendingCountrySelection -= country
      }
    }
  }

  def checkSelection(pendingCountrySelection: mutable.Set[Country]): Boolean =
    validCheck(parent.game, pendingCountrySelection.toSet)

  def updatePendingCountrySelection(): Unit = {
    tableModel.setRowCount(0)
    for (country <- pendingCountrySelection) {
      tableModel.addRow(Array[Object](new CountryDelegate(country),
        f"${parent.game.influence(country, Faction.US)} : ${parent.game.influence(country, Faction.USSR)}"))
    }
    val rest = point - pendingCountrySelection.size
    text(0) = String.format(tip, rest.toString, point2.toString)
    buttonDone.setEnabled(!mustAllPoints || rest == 0)
    if (tableModel.getRowCount > 0) {
      table.setRowSelectionInterval(0, 0)
    }
    updateListeners.foreach(_())
    repaint()
  }

  override def actionPerformed(e: ActionEvent): Unit = {
    e.getSource match {
      case this.buttonReset =>
        pendingCountrySelection.clear()
        updatePendingCountrySelection()
      case this.buttonCancel =>
        val selectedRow = table.getSelectedRow
        if (selectedRow >= 0) {
          val country = tableModel.getValueAt(selectedRow, 0).asInstanceOf[CountryDelegate].country
          pendingCountrySelection -= country
          updatePendingCountrySelection()
          if (tableModel.getRowCount > selectedRow) {
            table.setRowSelectionInterval(selectedRow, selectedRow)
          }
        }
      case this.buttonDone =>
        val op = new OperationSelectCountry(parent.game.playerId, parent.game.playerFaction, pendingCountrySelection.toSet)
        pendingCountrySelection.clear()
        parent.operationListeners.foreach(_(op))
    }
  }

  override def reset(): Unit = {
    pendingCountrySelection.clear()
    updateListeners.foreach(_())
  }
}

class ControlSubUISelectCardOrCancel(parent: ControlUI)
  extends ControlSubUICard(parent, Array("")) {

  val buttonDone = addButton(Lang.done, 120, 70, 70, 30)
  val buttonCancel = addButton(Lang.cancel, 120, 130, 70, 30)

  override def updateCard(): Unit = {
    buttonDone.setEnabled(card.id != 0)
  }

  override def actionPerformed(e: ActionEvent): Unit = {
    val cardOption = e.getSource match {
      case this.buttonDone => Some(this.card)
      case this.buttonCancel => None
    }
    val op = new OperationSelectCard(parent.game.playerId, parent.game.playerFaction, cardOption)
    parent.operationListeners.foreach(_(op))
  }
}

class ControlSubUIYesNo(parent: ControlUI) extends
  ControlSubUIText(parent, Array("", "", "")) {

  val buttonYes = addButton(Lang.yes, 20, 120, 70, 30)
  val buttonNo  = addButton(Lang.no, 110, 120, 70, 30)

  override def actionPerformed(e: ActionEvent): Unit = {
    val result = e.getSource == buttonYes
    val op = new OperationYesNo(parent.game.playerId, parent.game.playerFaction, result)
    parent.operationListeners.foreach(_(op))
  }
}

class ControlSubUIConfirm(parent: ControlUI) extends
  ControlSubUIText(parent, Array("", "", "")) {

  val buttonConfirm = addButton(Lang.done, 65, 120, 70, 30)

  override def actionPerformed(e: ActionEvent): Unit = {
    val op = new OperationYesNo(parent.game.playerId, parent.game.playerFaction, true)
    parent.operationListeners.foreach(_(op))
  }
}

class ControlSubUISelectRegion(parent: ControlUI)
  extends ControlSubUIText(parent, Array("", Lang.selectRegion)) {

  val buttons = Map(
    addButton(Lang.getRegionName(Region.Europe), 25, 85, 70, 30) -> Region.Europe,
    addButton(Lang.getRegionName(Region.Asia), 105, 85, 70, 30) -> Region.Asia,
    addButton(Lang.getRegionName(Region.MidEast), 25, 120, 70, 30) -> Region.MidEast,
    addButton(Lang.getRegionName(Region.Africa), 105, 120, 70, 30) -> Region.Africa,
    addButton(Lang.getRegionName(Region.MidAmerica), 25, 155, 70, 30) -> Region.MidAmerica,
    addButton(Lang.getRegionName(Region.SouthAmerica), 105, 155, 70, 30) -> Region.SouthAmerica
  )

  override def actionPerformed(e: ActionEvent): Unit = {
    val region = buttons(e.getSource.asInstanceOf[JButton])
    val op = new OperationSelectRegion(parent.game.playerId, parent.game.playerFaction, region)
    parent.operationListeners.foreach(_(op))
  }
}

class ControlSubUISelectMultipleCards(parent: ControlUI)
  extends ControlSubUIText(parent, Array("", "", "")) {

  var pendingCardSelection: mutable.Set[Card] = null
  val buttonConfirm = addButton(Lang.done, 65, 120, 70, 30)

  var updateListeners: List[() => Unit] = List()

  def toggleCard(card: Card) = {
    if (pendingCardSelection(card)) {
      pendingCardSelection -= card
    } else {
      pendingCardSelection += card
    }
    updateListeners.foreach(_())
  }

  override def actionPerformed(e: ActionEvent): Unit = {
    val op = new OperationSelectCards(parent.game.playerId, parent.game.playerFaction, pendingCardSelection.toSet)
    parent.operationListeners.foreach(_(op))
    pendingCardSelection.clear()
  }

  override def reset(): Unit = {
    pendingCardSelection.clear()
    updateListeners.foreach(_())
  }
}

class ControlSubUISummit(parent: ControlUI) extends ControlSubUIText(parent, Array("", "", Lang.cardTips(Card045Summit)(0))) {

  val buttonImprove = addButton(Lang.improve, 10, 120, 60, 30)
  val buttonKeep = addButton(Lang.keep, 70, 120, 60, 30)
  val buttonDegrade = addButton(Lang.degrade, 130, 120, 60, 30)

  override def actionPerformed(e: ActionEvent): Unit = {
    val v = e.getSource match {
      case this.buttonImprove => 1
      case this.buttonKeep => 0
      case this.buttonDegrade => -1
    }
    val op = new OperationIntValue(parent.game.playerId, parent.game.playerFaction, v)
    parent.operationListeners.foreach(_(op))
  }
}

class ControlSubUIHowILearnStopWorry(parent: ControlUI)
  extends ControlSubUIText(parent, Array("", "", Lang.cardTips(Card046HowILearnStopWorry)(0))) {

  val buttons = (1 to 5).map(i => addButton(i.toString, 5 + (5 - i) * 38, 120, 38, 30) -> i).toMap

  override def actionPerformed(e: ActionEvent): Unit = {
    val v = buttons(e.getSource.asInstanceOf[JButton])
    val op = new OperationIntValue(parent.game.playerId, parent.game.playerFaction, v)
    parent.operationListeners.foreach(_(op))
  }
}

class ControlSubUIGrainSales(parent: ControlUI)
  extends ControlSubUICard(parent, Array(Lang.cardTips(Card067GrainSales)(0))) {

  val buttonPlay = addButton(Lang.play, 120, 70, 70, 30)
  val buttonReturn = addButton(Lang.giveBack, 120, 130, 70, 30)

  override def updateCard(): Unit = {
    buttonPlay.setEnabled(card.id != 0)
  }

  override def actionPerformed(e: ActionEvent): Unit = {
    val v = e.getSource match {
      case this.buttonPlay => true
      case this.buttonReturn => false
    }
    val op = new OperationYesNo(parent.game.playerId, parent.game.playerFaction, v)
    parent.operationListeners.foreach(_(op))
  }
}
