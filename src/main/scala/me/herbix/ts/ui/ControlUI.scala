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
    val SelectOperation = Value
    val SelectCountry = Value
    val SelectCardOrCancel = Value
    val YesNo = Value
    val Confirm = Value
    val Summit = Value
    val StopWorry = Value
  }

  import UIType._

  var uiType = Waiting

  val uiChooseFaction = new ControlSubUIChooseFaction(this)
  val uiWaiting = new ControlSubUIText(this, Array("", "", Lang.waitingForOpposite))
  val uiInfluence = new ControlSubUIModifyInfluence(this)
  val uiSelectCard = new ControlSubUISelectCard(this)
  val uiSelectCardAndAction = new ControlSubUISelectCardAndAction(this)
  val uiSelectOperation = new ControlSubUISelectOperation(this)
  val uiSelectCountry = new ControlSubUISelectCountry(this)
  val uiSelectCardOrCancel = new ControlSubUISelectCardOrCancel(this)
  val uiYesNo = new ControlSubUIYesNo(this)
  val uiConfirm = new ControlSubUIConfirm(this)
  val uiSummit = new ControlSubUISummit(this)
  val uiStopWorry = new ControlSubUIHowILearnStopWorry(this)

  var operationListeners: List[Operation => Unit] = List()

  setLayout(new CardLayout)
  add(uiChooseFaction, ChooseFaction.toString)
  add(uiWaiting, Waiting.toString)
  add(uiInfluence, Influence.toString)
  add(uiSelectCard, SelectCard.toString)
  add(uiSelectCardAndAction, SelectCardAndAction.toString)
  add(uiSelectOperation, SelectOperation.toString)
  add(uiSelectCountry, SelectCountry.toString)
  add(uiSelectCardOrCancel, SelectCardOrCancel.toString)
  add(uiYesNo, YesNo.toString)
  add(uiConfirm, Confirm.toString)
  add(uiSummit, Summit.toString)
  add(uiStopWorry, StopWorry.toString)

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
          addInfluenceUI(6, Lang.putEastEurope, true, true, true, false, game.playerFaction, _.forall(_._1.regions(Region.EastEurope)))
        } else {
          waitOtherUI()
        }
      case State.putStartUS =>
        if (game.playerFaction == Faction.US) {
          addInfluenceUI(7, Lang.putWestEurope, true, true, true, false, game.playerFaction, _.forall(_._1.regions(Region.WestEurope)))
        } else {
          waitOtherUI()
        }
      case State.putStartUSExtra =>
        if (game.playerFaction == Faction.US) {
          addInfluenceUI(1, Lang.putExtra, true, true, true, false, game.playerFaction, _.forall(_._1.influence(Faction.US) > 0))
        } else {
          waitOtherUI()
        }
      case State.selectHeadlineCard =>
        if (!game.flags.hasFlag(game.playerFaction, Flags.SpaceAwardHeadlineThen)) {
          selectCardUI(Lang.selectHeadline)
        } else {
          waitOtherUI()
        }
      case State.selectHeadlineCard2 =>
        if (game.flags.hasFlag(game.playerFaction, Flags.SpaceAwardHeadlineThen)) {
          selectCardUI(Lang.selectHeadline)
        } else {
          waitOtherUI()
        }
      case State.selectCardAndAction =>
        if (game.playerFaction == game.operatingPlayer) {
          selectCardAndActionUI()
        } else {
          waitOtherUI()
        }
      case State.cardOperationSelect =>
        if (game.playerFaction == game.operatingPlayer) {
          selectOperationUI(Lang.operationSelect)
        } else {
          waitOtherUI()
        }
      case State.cardOperationAddInfluence =>
        if (game.playerFaction == game.operatingPlayer) {
          addInfluenceUI(game.currentCard.op, Lang.operationAddInfluence, true, false, true, true, game.playerFaction,
            game.addInfluenceCondition(game.playerFaction))
        } else {
          waitOtherUI()
        }
      case State.cardOperationRealignment =>
        if (game.playerFaction == game.operatingPlayer) {
          selectCountryUI(1, game.getCurrentRealignmentRest(game.playerFaction), Lang.operationRealignment, true,
            _.forall(game.canRealignment(game.playerFaction, _)))
        } else {
          waitOtherUI()
        }
      case State.cardOperationCoup =>
        if (game.playerFaction == game.operatingPlayer) {
          selectCountryUI(1, game.currentCard.op, Lang.operationCoup, true,
            _.forall(game.canCoup(game.playerFaction, _)))
        } else {
          waitOtherUI()
        }
      case State.discardHeldCard =>
        if (game.flags.hasFlag(game.playerFaction, Flags.SpaceAwardMayDiscard)) {
          selectCardOrCancelUI(Lang.discardHeldCard)
        } else {
          waitOtherUI()
        }
      case State.selectTake8Rounds =>
        if (game.playerFaction == game.phasingPlayer) {
          yesNoUI(Lang.take8rounds)
        } else {
          waitOtherUI()
        }
      case State.quagmireDiscard =>
        if (game.playerFaction == game.phasingPlayer) {
          selectCardUI(Lang.selectQuagmireDiscard)
        } else {
          waitOtherUI()
        }
      case State.quagmirePlayScoringCard =>
        if (game.playerFaction == game.phasingPlayer) {
          selectCardUI(Lang.selectQuagmireScoringCard)
        } else {
          waitOtherUI()
        }
      case State.cardEventInfluence =>
        if (game.playerFaction == game.operatingPlayer) {
          val card = game.currentCard.asInstanceOf[CardNeedsSelection]
          val step = card.getStep(game)
          val stepMeta = card.getStepMeta(game).asInstanceOf[(Int, Boolean, Boolean, Faction, Map[Country, Int] => Boolean)]
          addInfluenceUI(stepMeta._1, Lang.cardTips(card)(step-1), stepMeta._2, true, stepMeta._3, false, stepMeta._4, stepMeta._5)
        } else {
          waitOtherUI()
        }
      case State.cardEventSelectCardOrCancel =>
        if (game.playerFaction == game.operatingPlayer) {
          val card = game.currentCard.asInstanceOf[CardNeedsSelection]
          val step = card.getStep(game)
          selectCardOrCancelUI(Lang.cardTips(card)(step-1))
        } else {
          waitOtherUI()
        }
      case State.cardEventYesNo =>
        if (game.playerFaction == game.operatingPlayer) {
          val card = game.currentCard.asInstanceOf[CardNeedsSelection]
          val step = card.getStep(game)
          yesNoUI(Lang.cardTips(card)(step-1))
        } else {
          waitOtherUI()
        }
      case State.cardEventSelectCountry =>
        if (game.playerFaction == game.operatingPlayer) {
          val card = game.currentCard.asInstanceOf[CardNeedsSelection]
          val step = card.getStep(game)
          val stepMeta = card.getStepMeta(game).asInstanceOf[(Int, Boolean, Set[Country] => Boolean)]
          val rest = if (game.currentCardData != null && game.currentCardData.isInstanceOf[Int])
            game.currentCardData.asInstanceOf[Int]
          else
            0
          selectCountryUI(stepMeta._1, rest, Lang.cardTips(card)(step-1), stepMeta._2, stepMeta._3)
        } else {
          waitOtherUI()
        }
      case State.cardEventConfirm =>
        if (game.playerFaction == game.operatingPlayer) {
          val card = game.currentCard.asInstanceOf[CardNeedsSelection]
          val step = card.getStep(game)
          confirmUI(Lang.cardTips(card)(step-1))
        } else {
          waitOtherUI()
        }
      case State.cardEventSelectCard =>
        if (game.playerFaction == game.operatingPlayer) {
          val card = game.currentCard.asInstanceOf[CardNeedsSelection]
          val step = card.getStep(game)
          selectCardUI(Lang.cardTips(card)(step-1))
        } else {
          waitOtherUI()
        }
      case State.cardEventSpecial =>
        if (game.playerFaction == game.operatingPlayer) {
          val card = game.currentCard.asInstanceOf[CardNeedsSelection]
          val step = card.getStep(game)
          card match {
            case Card045Summit => showSubUI(Summit)
            case Card046HowILearnStopWorry => showSubUI(StopWorry)
            case Card047Junta =>
              selectOperationUI(Lang.operationSelect)
              uiSelectOperation.influence.setEnabled(false)
              uiSelectOperation.coup.setEnabled(Card047Junta.canCoup(game))
          }
        } else {
          waitOtherUI()
        }
      case _ => waitOtherUI()
    }
    repaint()
  }

  def chooseFactionUI(): Unit = showSubUI(ChooseFaction)
  def waitOtherUI(): Unit = showSubUI(Waiting)

  def addInfluenceUI(point: Int, tip: String, isAdd: Boolean, ignoreControl: Boolean,
                     mustAllPoints: Boolean, modifyOp: Boolean, targetFaction: Faction, valid: Map[Country, Int] => Boolean) = {
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

  def selectCardAndActionUI() = {
    showSubUI(SelectCardAndAction)
    uiSelectCardAndAction.resetCard()
  }

  def selectOperationUI(tip: String) = {
    showSubUI(SelectOperation)
    uiSelectOperation.text(1) = String.format(tip, game.modifyOp(game.playerFaction, game.currentCard.op).toString)
    uiSelectOperation.influence.setEnabled(true)
    uiSelectOperation.realignment.setEnabled(true)
    uiSelectOperation.coup.setEnabled(game.canCoup(game.playerFaction))
  }

  def selectCountryUI(point: Int, point2: Int, tip: String, mustAllPoints: Boolean, valid: Set[Country] => Boolean): Unit = {
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
  ControlSubUIText(parent, Array("")) { // TODO what to do when no avaliable target

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
  var mustAllPoints = true
  var modifyOp = true
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
    val rest = getPoint -
      parent.game.calculateInfluenceCost(pendingInfluenceChange, parent.game.playerFaction, ignoreControl)
    text(0) = String.format(tip, rest.toString)
    buttonDone.setEnabled(rest == 0 || !mustAllPoints)
    if (tableModel.getRowCount > 0) {
      table.setRowSelectionInterval(0, 0)
    }
    updateListeners.foreach(_())
    repaint()
  }

  def getPoint: Int =
    if (modifyOp) parent.game.modifyOp(parent.game.playerFaction, point, pendingInfluenceChange.keys) else point

  def checkInfluence(): Boolean = {
    parent.game.calculateInfluenceCost(pendingInfluenceChange, parent.game.playerFaction, ignoreControl) <=
      getPoint &&
      validCheck(pendingInfluenceChange.toMap) &&
      (isAdd || pendingInfluenceChange.forall(e => e._1.influence(targetFaction) >= e._2))
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
    val op = new OperationSelectCard(parent.game.playerId, parent.game.playerFaction, card)
    parent.operationListeners.foreach(_(op))
    resetCard()
  }

}

class ControlSubUISelectCardAndAction(parent: ControlUI)
  extends ControlSubUICard(parent, Array(Lang.selectCardAndAction)) {

  val buttonSpace = addButton(Lang.space, 120, 50, 70, 30)
  val buttonEvent = addButton(Lang.event, 120, 100, 70, 30)
  val buttonOperation = addButton(Lang.operation, 120, 150, 70, 30)

  override def updateCard(): Unit = {
    val eventEnabled = parent.game.canCardEvent(card, parent.game.playerFaction)

    buttonSpace.setEnabled(parent.game.canCardSpace(card, parent.game.playerFaction))
    buttonEvent.setEnabled(eventEnabled)
    buttonOperation.setEnabled(parent.game.canCardOperation(card, parent.game.playerFaction))

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
  val buttonCancel = addButton(Lang.cancel, 70, 165, 60, 30)
  val buttonReset = addButton(Lang.reset, 130, 165, 60, 30)

  val tableModel = new DefaultTableModel()
  val table = new JTable(tableModel)
  val tableOuter = new JScrollPane(table)

  var tip = "%s(%s)"
  var point = 0
  var point2 = 0
  var mustAllPoints = true
  var validCheck: Set[Country] => Boolean = null

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
      if (checkSelection()) {
        updatePendingCountrySelection()
      } else {
        pendingCountrySelection.clear()
        pendingCountrySelection += oldCountry
      }

    } else {
      pendingCountrySelection += country
      if (checkSelection()) {
        updatePendingCountrySelection()
      } else {
        pendingCountrySelection -= country
      }
    }
  }

  def checkSelection(): Boolean = {
    point - pendingCountrySelection.size >= 0 && validCheck(pendingCountrySelection.toSet)
  }

  def updatePendingCountrySelection(): Unit = {
    tableModel.setRowCount(0)
    for (country <- pendingCountrySelection) {
      tableModel.addRow(Array[Object](new CountryDelegate(country),
        f"${country.influence(Faction.US)} : ${country.influence(Faction.USSR)}"))
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
}

class ControlSubUISelectCardOrCancel(parent: ControlUI)
  extends ControlSubUICard(parent, Array("")) {

  val buttonDone = addButton(Lang.done, 120, 70, 70, 30)
  val buttonCancel = addButton(Lang.cancel, 120, 130, 70, 30)

  override def updateCard(): Unit = {
    buttonDone.setEnabled(card.id != 0)
  }

  override def actionPerformed(e: ActionEvent): Unit = {
    val card = e.getSource match {
      case this.buttonDone => this.card
      case this.buttonCancel => null
    }
    val op = new OperationSelectCard(parent.game.playerId, parent.game.playerFaction, card)
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
