package me.herbix.ts.client

import java.util.Random
import javax.swing.{UIManager, WindowConstants}

import me.herbix.ts.client.NewRoomDialog.GameVariantDelegate
import me.herbix.ts.logic.{Faction, GameFactory, GameVariant}
import me.herbix.ts.ui.GameUI
import me.herbix.ts.util.Resource

/**
  * Created by Chaofan on 2016/7/9.
  */
object ClientLocal extends App {

  UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName)

  var extraInfluence = 0
  var hasOptional = false
  var drawWinner = Faction.Neutral
  var gameVariant = GameVariant.Standard

  var gameUI1: GameUI = null
  var gameUI2: GameUI = null

  @volatile
  var uiInitialized = false

  new Thread() {
    override def run(): Unit = {
      val time = System.nanoTime()
      ClientLocal.synchronized {
        Resource.getClass
        gameUI1 = new GameUI(0)
        gameUI2 = new GameUI(1)
        gameUI1.debugMode = true
        gameUI2.debugMode = true
        uiInitialized = true
        ClientLocal.notify()
      }
      println((System.nanoTime() - time) / 1e9)
    }
  }.start()

  NewRoomDialog.setVisible(true)

  if (NewRoomDialog.isDone) {
    extraInfluence = NewRoomDialog.slider.getValue
    drawWinner = if (NewRoomDialog.us.isSelected) Faction.US else Faction.USSR
    hasOptional = NewRoomDialog.optional.isSelected
    gameVariant = NewRoomDialog.variant.getSelectedItem.asInstanceOf[GameVariantDelegate].gameVariant
  } else {
    System.exit(0)
  }

  ClientLocal.synchronized {
    if (!uiInitialized) {
      ClientLocal.wait()
    }
  }

  gameUI1.init(GameFactory.createGameByVariant(gameVariant))
  gameUI2.init(GameFactory.createGameByVariant(gameVariant))

  val game1 = gameUI1.game
  val game2 = gameUI2.game

  game1.extraInfluence = extraInfluence
  game1.drawGameWinner = drawWinner
  game1.optionalCards = hasOptional

  game2.extraInfluence = extraInfluence
  game2.drawGameWinner = drawWinner
  game2.optionalCards = hasOptional

  game1.anotherGame = game2
  game2.anotherGame = game1

  val seed = new Random().nextLong()
  game1.setRandomSeed(seed)
  game2.setRandomSeed(seed)

  gameUI1.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
  gameUI2.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
  gameUI1.setVisible(true)
  gameUI2.setVisible(true)
}
