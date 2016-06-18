package me.herbix.ts.ui

import java.awt.event.{ComponentEvent, ComponentListener, WindowEvent, WindowListener}
import java.awt.{Color, Dimension, ScrollPane, BorderLayout}
import javax.swing._

import me.herbix.ts.logic.Game

/**
  * Created by Chaofan on 2016/6/13.
  */
object GameUI extends JFrame {

  val game = new Game

  UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName)

  setLayout(new BorderLayout)

  val bgColor = new Color(225, 225, 225) // new Color(213, 189, 181)

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
  controlUI.setPreferredSize(new Dimension(200, 220))
  controlUI.setBackground(bgColor)
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

  val flagsUI = new FlagsUI
  flagsUI.setBackground(bgColor)
  rightPanel.add(flagsUI)

  pack()

  def main(args: Array[String]) {
    GameUI.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
    GameUI.setVisible(true)
  }

}
