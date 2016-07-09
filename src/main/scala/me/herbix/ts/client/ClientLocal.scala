package me.herbix.ts.client

import java.util.Random
import javax.swing.{UIManager, WindowConstants}

import me.herbix.ts.client.NewRoomDialog.GameVariantDelegate
import me.herbix.ts.logic.{Faction, GameVariant}
import me.herbix.ts.ui.GameUI

/**
  * Created by Chaofan on 2016/7/9.
  */
object ClientLocal extends App {

  UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName)

  var extraInfluence = 0
  var hasOptional = false
  var drawWinner = Faction.Neutral
  var gameVariant = GameVariant.Standard

  var GameUI1: GameUI = null
  var GameUI2: GameUI = null

  @volatile
  var uiInitialized = false

  new Thread() {
    override def run(): Unit = {
      val time = System.nanoTime()
      ClientLocal.synchronized {
        GameUI1 = new GameUI(0)
        GameUI2 = new GameUI(1)
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

  val game1 = GameUI1.game
  val game2 = GameUI2.game

  game1.extraInfluence = extraInfluence
  game1.drawGameWinner = drawWinner
  game1.optionalCards = hasOptional
  game1.gameVariant = gameVariant

  game2.extraInfluence = extraInfluence
  game2.drawGameWinner = drawWinner
  game2.optionalCards = hasOptional
  game2.gameVariant = gameVariant

  game1.anotherGame = game2
  game2.anotherGame = game1

  val seed = new Random().nextLong()
  game1.randomSeed = seed
  game2.randomSeed = seed
  game1.random.setSeed(seed)
  game2.random.setSeed(seed)

  GameUI1.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
  GameUI2.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
  GameUI1.setVisible(true)
  GameUI2.setVisible(true)
}
