package me.herbix.ts.ui

import java.awt.event._
import java.awt.{Color, Dimension, ScrollPane, BorderLayout}
import java.util.Random
import javax.swing._

import me.herbix.ts.logic.Faction.Faction
import me.herbix.ts.logic._
import scala.collection.mutable

/**
  * Created by Chaofan on 2016/6/13.
  */
class GameUI(playerId: Int) extends JFrame {

  val game = new Game

  // UI start
  setLayout(new BorderLayout)

  val bgColor = getBackground // new Color(213, 189, 181)

  val leftPanel = new JPanel
  leftPanel.setPreferredSize(new Dimension(200, 100))
  leftPanel.setBackground(bgColor)
  leftPanel.setLayout(new BorderLayout)
  add(leftPanel, BorderLayout.WEST)

  val infoUI = new InfoUI(game)
  infoUI.setPreferredSize(new Dimension(200, 150))
  infoUI.setBackground(bgColor)
  leftPanel.add(infoUI, BorderLayout.SOUTH)

  val controlUI = new ControlUI(game)
  controlUI.setPreferredSize(new Dimension(200, 200))
  controlUI.setBackground(bgColor)
  controlUI.operationListeners :+= ((op: Operation) => if (op != null) game.sendNextState(op))
  leftPanel.add(controlUI, BorderLayout.NORTH)

  val historyUI = new HistoryUI(game)
  historyUI.setBackground(bgColor)
  val historyUIOuter = new JScrollPane(historyUI)
  historyUIOuter.setBorder(null)
  historyUIOuter.getVerticalScrollBar.setUnitIncrement(40)
  leftPanel.add(historyUIOuter)

  val centerPanel = new JPanel
  centerPanel.setLayout(new BorderLayout)
  add(centerPanel)

  val worldMapUI = new WorldMapUI(game)
  val worldMapUIOuter = new JScrollPane(worldMapUI)
  worldMapUI.setOuter(worldMapUIOuter)
  worldMapUIOuter.setPreferredSize(new Dimension(750, 480))
  worldMapUIOuter.setWheelScrollingEnabled(false)
  worldMapUIOuter.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER)
  worldMapUIOuter.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_NEVER)
  centerPanel.add(worldMapUIOuter)

  val handUI = new HandUI(game)
  handUI.setPreferredSize(new Dimension(600, 150))
  handUI.setBackground(bgColor)
  centerPanel.add(handUI, BorderLayout.SOUTH)

  val rightPanel = new JPanel
  rightPanel.setPreferredSize(new Dimension(200, 100))
  rightPanel.setBackground(bgColor)
  rightPanel.setLayout(new BorderLayout)
  add(rightPanel, BorderLayout.EAST)

  val detailUI = new DetailUI
  detailUI.setPreferredSize(new Dimension(200, 300))
  rightPanel.add(detailUI, BorderLayout.NORTH)

  val flagsUI = new FlagsUI(game)
  flagsUI.setBackground(bgColor)
  rightPanel.add(flagsUI)

  game.playerId = playerId
  setTitle("Player - " + game.playerId)

  pack()
  // UI end

  val pendingInfluenceChange: mutable.Map[Country, Int] = mutable.Map()
  controlUI.uiInfluence.pendingInfluenceChange = pendingInfluenceChange
  worldMapUI.pendingInfluenceChange = pendingInfluenceChange

  worldMapUI.countryClickListeners :+= ((country: Country, button: Int) => {
    if (button == MouseEvent.BUTTON1) {
      controlUI.uiType match {
        case controlUI.UIType.Influence => controlUI.uiInfluence.addInfluence(country, 1)
        case controlUI.UIType.SelectCountry => controlUI.uiSelectCountry.toggleCountry(country)
        case _ =>
      }
    }
  })

  handUI.cardClickListeners :+= ((card: Card) => {
    val hasEventCards = handUI.hasEventCards
    if ((handUI.selfHand.isSelected && !hasEventCards) || (handUI.eventCards.isSelected && hasEventCards)) {
      controlUI.uiType match {
        case controlUI.UIType.SelectCard =>
          controlUI.uiSelectCard.setCard(card)
        case controlUI.UIType.SelectCardAndAction =>
          controlUI.uiSelectCardAndAction.setCard(card)
        case controlUI.UIType.SelectCardOrCancel =>
          controlUI.uiSelectCardOrCancel.setCard(card)
        case controlUI.UIType.SelectMultipleCards =>
          controlUI.uiSelectMultipleCards.toggleCard(card)
        case _ =>
      }
    }
  })

  controlUI.uiInfluence.updateListeners :+= (() => {
    worldMapUI.pendingInfluenceFaction = controlUI.uiInfluence.targetFaction
    worldMapUI.pendingInfluenceIsAdd = controlUI.uiInfluence.isAdd
    worldMapUI.repaint()
  })

  worldMapUI.countryHoverListeners :+= ((country: Country) => {
    detailUI.setCountry(country)
  })

  handUI.cardHoverListeners :+= ((card: Card) => {
    detailUI.setCard(card)
  })

  ControlSubUICard.cardHoverListeners :+= ((card: Card) => {
    detailUI.setCard(card)
  })

  flagsUI.flagHoverListeners :+= ((faction: Faction, flag: Flag, flagData: Any) => {
    detailUI.setFlag(faction, flag, flagData)
  })

  val pendingCountrySelection: mutable.Set[Country] = mutable.Set()
  controlUI.uiSelectCountry.pendingCountrySelection = pendingCountrySelection
  worldMapUI.pendingCountrySelection = pendingCountrySelection

  controlUI.uiSelectCountry.updateListeners :+= (() => {
    worldMapUI.repaint()
  })

  historyUI.historyHoverListeners :+= ((history: History) => {
    history match {
      case h: HistoryCard =>
        detailUI.setCard(h.card)
      case h: HistoryFlag =>
        detailUI.setFlag(h.faction, h.flag, h.data)
      case _ =>
    }
  })

  val pendingCardSelection: mutable.Set[Card] = mutable.Set()
  controlUI.uiSelectMultipleCards.pendingCardSelection = pendingCardSelection
  handUI.pendingCardSelection = pendingCardSelection

  controlUI.uiSelectMultipleCards.updateListeners :+= (() => {
    handUI.repaint()
  })

  flagsUI.flagClickListeners :+= ((faction: Faction, flag: Flag, flagData: Any) => {
    if (faction == game.playerFaction && flag == Flags.CubaMissile && game.stateStack.top != State.end) {
      val op = new OperationCubaMissileRequest(game.playerId, game.playerFaction, false)
      game.sendNextState(op)
    }
  })


}

/*
object GameUI {
  def main(args: Array[String]) {

    UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName)

    val GameUI1 = new GameUI(0)
    val GameUI2 = new GameUI(1)

    GameUI1.game.anotherGame = GameUI2.game
    GameUI2.game.anotherGame = GameUI1.game

    val seed = new Random().nextLong()
    GameUI1.game.randomSeed = seed
    GameUI2.game.randomSeed = seed
    GameUI1.game.random.setSeed(seed)
    GameUI2.game.random.setSeed(seed)

    GameUI1.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
    GameUI2.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
    GameUI1.setVisible(true)
    GameUI2.setVisible(true)
  }
}
*/