package me.herbix.ts.ui

import java.awt.event.{ComponentEvent, ComponentListener, WindowEvent, WindowListener}
import java.awt.{Color, Dimension, ScrollPane, BorderLayout}
import java.util.Random
import javax.swing._

import me.herbix.ts.logic.Game

/**
  * Created by Chaofan on 2016/6/13.
  */
class GameUI(playerId: Int) extends JFrame {

  val game = new Game

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
  game.stateUpdateListeners = game.stateUpdateListeners :+ (() => infoUI.repaint())
  leftPanel.add(infoUI, BorderLayout.SOUTH)

  val controlUI = new ControlUI(game)
  controlUI.setPreferredSize(new Dimension(200, 220))
  controlUI.setBackground(bgColor)
  game.stateUpdateListeners = game.stateUpdateListeners :+ (() => controlUI.updateState())
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
  game.stateUpdateListeners = game.stateUpdateListeners :+ (() => worldMapUI.repaint())
  worldMapUIOuter.setPreferredSize(new Dimension(750, 480))
  worldMapUIOuter.setWheelScrollingEnabled(false)
  worldMapUIOuter.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER)
  worldMapUIOuter.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_NEVER)
  centerPanel.add(worldMapUIOuter)

  val handUI = new HandUI(game)
  handUI.setPreferredSize(new Dimension(600, 150))
  handUI.setBackground(bgColor)
  game.stateUpdateListeners = game.stateUpdateListeners :+ (() => handUI.updateState())
  centerPanel.add(handUI, BorderLayout.SOUTH)

  val rightPanel = new JPanel
  rightPanel.setPreferredSize(new Dimension(200, 100))
  rightPanel.setBackground(bgColor)
  rightPanel.setLayout(new BorderLayout)
  add(rightPanel, BorderLayout.EAST)

  val detailUI = new DetailUI
  detailUI.setPreferredSize(new Dimension(200, 300))
  rightPanel.add(detailUI, BorderLayout.NORTH)

  val flagsUI = new FlagsUI
  flagsUI.setBackground(bgColor)
  rightPanel.add(flagsUI)

  game.playerId = playerId
  setTitle("Player - " + game.playerId)

  pack()

}

object GameUI {
  def main(args: Array[String]) {
    val GameUI1 = new GameUI(0)
    val GameUI2 = new GameUI(1)

    GameUI1.game.anotherGame = GameUI2.game
    GameUI2.game.anotherGame = GameUI1.game

    val seed = new Random().nextLong()
    GameUI1.game.dice.setSeed(seed)
    GameUI2.game.dice.setSeed(seed)

    GameUI1.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
    GameUI1.setVisible(true)
    GameUI2.setVisible(true)
  }
}
