package me.herbix.ts.ui

import java.awt._
import javax.swing.JPanel

import me.herbix.ts.logic.{Faction, Game}
import me.herbix.ts.util.Lang

/**
  * Created by Chaofan on 2016/6/17.
  */
class InfoUI(val game: Game) extends JPanel {

  game.stateUpdateListeners :+= (() => repaint())

  import me.herbix.ts.util.Resource._

  override def paint(g: Graphics): Unit = {
    super.paint(g)

    g.asInstanceOf[Graphics2D].setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON)
    g.setFont(textFont)
    g.setColor(textColor)
    g.drawString(Lang.currentPlayer, 10, 28)
    g.drawString(String.format(Lang.turn, game.turn.toString, game.round.toString), 10, 58)
    g.drawString(String.format(Lang.deck, game.deck.cardCount.toString), 100, 58)
    g.drawString(Lang.space + " : ", 10, 88)
    g.drawString(Lang.military + " : ", 100, 88)
    g.drawString(String.format(Lang.defcon, game.defcon.toString), 10, 118)
    g.drawString(Lang.vp, 100, 118)

    val fm = g.getFontMetrics
    val w1 = fm.stringWidth(Lang.space)
    val w2 = fm.stringWidth(" :")

    g.setColor(usColor)
    g.drawString(game.space(Faction.US).toString, 10 + w1, 88)
    g.drawString(game.military(Faction.US).toString, 100 + w1, 88)

    g.setColor(ussrColor)
    g.drawString(game.space(Faction.USSR).toString, 10 + w1 + w2, 88)
    g.drawString(game.military(Faction.USSR).toString, 100 + w1 + w2, 88)

    val w3 = fm.stringWidth(Lang.currentPlayer)
    if (game.currentPlayer == Faction.US) {
      g.setColor(usColor)
      g.drawString(Lang.US, 10 + w3, 28)
    } else if (game.currentPlayer == Faction.USSR) {
      g.setColor(ussrColor)
      g.drawString(Lang.USSR, 10 + w3, 28)
    }

    val w4 = fm.stringWidth(Lang.vp)
    if (game.vp > 0) {
      g.setColor(usColor)
      g.drawString(game.vp.toString, 100 + w4, 118)
    } else if (game.vp < 0) {
      g.setColor(ussrColor)
      g.drawString((-game.vp).toString, 100 + w4, 118)
    } else {
      g.setColor(textColor)
      g.drawString("0", 100 + w4, 118)
    }

  }

}
