// Copyright (C) 2017 Chaofan

package me.herbix.ts.client

import java.util.Random
import javax.swing.{SwingUtilities, UIManager, WindowConstants}

import me.herbix.ts.agent.AgentFactory
import me.herbix.ts.client.NewRoomDialog.GameVariantDelegate
import me.herbix.ts.logic._
import me.herbix.ts.ui.GameUI
import me.herbix.ts.util.{OperationHint, Resource}

/**
  * Created by Chaofan on 2016/7/9.
  */
object ClientAgent extends App {

  UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName)

  var extraInfluence = 0
  var hasOptional = false
  var hasPromo1 = false
  var hasPromo2 = false
  var drawWinner = Faction.Neutral
  var gameVariant = GameVariant.Standard

  var gameUI1: GameUI = null

  @volatile
  var uiInitialized = false

  new Thread() {
    override def run(): Unit = {
      val time = System.nanoTime()
      ClientLocal.synchronized {
        Resource.getClass
        gameUI1 = new GameUI(0)
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
    hasPromo1 = NewRoomDialog.promo1.isSelected
    hasPromo2 = NewRoomDialog.promo2.isSelected
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

  val game1 = gameUI1.game
  val game2 = GameFactory.createGameByVariant(gameVariant)

  game2.playerId = 1

  game1.extraInfluence = extraInfluence
  game1.drawGameWinner = drawWinner
  game1.optionalCards = hasOptional
  game1.promo1Cards = hasPromo1
  game1.promo2Cards = hasPromo2

  game2.extraInfluence = extraInfluence
  game2.drawGameWinner = drawWinner
  game2.optionalCards = hasOptional
  game2.promo1Cards = hasPromo1
  game2.promo2Cards = hasPromo2

  game1.anotherGame = game2
  game2.anotherGame = game1

  val seed = new Random().nextLong()
  game1.setRandomSeed(seed)
  game2.setRandomSeed(seed)

  AgentFactory.createDefaultAgent(game2, (hint: OperationHint, input: Operation) => {
    SwingUtilities.invokeLater(new Runnable {
      override def run(): Unit = {
        if (hint == game2.getOperationHint) {
          game2.sendNextState(input)
        }
      }
    })
  })

  gameUI1.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
  gameUI1.setVisible(true)
}
