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
  UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName)
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

  val historyUI = new HistoryUI
  historyUI.setPreferredSize(new Dimension(180, 500))
  historyUI.setBackground(bgColor)
  val historyUIOuter = new JScrollPane(historyUI)
  historyUIOuter.setBorder(null)
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
    if (button == MouseEvent.BUTTON1 && controlUI.uiType == controlUI.UIType.Influence) {
      controlUI.uiInfluence.addInfluence(country, 1)
    }
  })
  handUI.cardClickListeners :+= ((card: Card) => {
    if (handUI.selfHand.isSelected || handUI.eventCards.isSelected) {
      controlUI.uiType match {
        case controlUI.UIType.SelectCard =>
          controlUI.uiSelectCard.setCard(card)
        case controlUI.UIType.SelectCardAndAction =>
          controlUI.uiSelectCardAndAction.setCard(card)
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
  controlUI.uiSelectCard.cardHoverListeners :+= ((card: Card) => {
    detailUI.setCard(card)
  })
  controlUI.uiSelectCardAndAction.cardHoverListeners :+= ((card: Card) => {
    detailUI.setCard(card)
  })
  flagsUI.flagHoverListeners :+= ((faction: Faction, flag: Flag) => {
    detailUI.setFlag(faction, flag)
  })

}

object GameUI {
  def main(args: Array[String]) {
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
