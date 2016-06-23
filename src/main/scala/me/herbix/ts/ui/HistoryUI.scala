package me.herbix.ts.ui

import java.awt.{Dimension, RenderingHints, Graphics2D, Graphics}
import javax.swing.JPanel

import me.herbix.ts.logic._
import me.herbix.ts.util.{Resource, Lang}

import scala.StringBuilder
import scala.collection.mutable

/**
  * Created by Chaofan on 2016/6/17.
  */
class HistoryUI(game: Game) extends JPanel {
  game.stateUpdateListeners :+= (() => updateContent())

  var historyText: mutable.Stack[HistoryMeta] = mutable.Stack()

  def updateContent(): Unit = {
    if (historyText.isEmpty) {
      historyText.pushAll(game.history.toStream.reverse.map(getHistoryMeta))
    } else {
      val myNewest = historyText.top.history
      historyText.pushAll(game.history.toStream.slice(0, game.history.indexOf(myNewest)).reverse.map(getHistoryMeta))
    }

    setPreferredSize(new Dimension(10, historyText.iterator.map(_.height).sum))
    getParent.revalidate()

    repaint()
  }

  def getHistoryMeta(history: History): HistoryMeta = {
    val str: String = history match {
      case h: HistoryStartGame => Lang.historyStartGame
      case h: HistoryTurnRound =>
        val str = if (h.round == 0) {
          String.format(Lang.historyTurnHeadline, h.turn.toString)
        } else {
          String.format(Lang.historyTurnRound, h.turn.toString, h.round.toString)
        }
        str + (if (h.faction == Faction.US) {
          " - " + Lang.US
        } else if (h.faction == Faction.USSR) {
          " - " + Lang.USSR
        } else {
          ""
        })
      case h: HistoryPickCard => String.format(Lang.historyPickCard, Lang.getName(h.faction), h.count.toString)
      case h: HistoryGetCard => String.format(Lang.historyGetCard, Lang.getName(h.faction), Lang.cardInfo(h.card.id)._1)
      case h: HistoryModifyInfluence =>
        val sb = new StringBuilder
        val change = if (h.isAdd) Lang.increase else Lang.decrease
        sb.append(String.format(Lang.historyModifyInfluence, Lang.getName(h.faction), change))
        sb.append('\n')
        for ((country, oldValue, newValue) <- h.detail) {
          sb.append(String.format(Lang.historyModifyInfluenceDetail, Lang.countryNames(country.name), oldValue.toString, newValue.toString))
          sb.append('\n')
        }
        sb.length -= 1
        sb.toString
      case h: HistoryPlayHeadline => String.format(Lang.historyPlayHeadline, Lang.getName(h.faction), Lang.cardInfo(h.card.id)._1)
      case h => h.toString
    }
    var height = 25

    val g = getGraphics
    val fm = g.getFontMetrics
    val w = getWidth
    var cx = 0

    for (ch <- str) {
      if (ch == '\n') {
        cx = 0
        height += 20
      } else {
        val chw = fm.charWidth(ch)
        if (cx + chw > w - 10) {
          cx = 0
          height += 18
        }
        cx += chw
      }
    }

    new HistoryMeta(str, height, history)
  }

  override def paint(graphics: Graphics): Unit = {
    super.paint(graphics)

    val g = graphics.asInstanceOf[Graphics2D]

    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    g.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BILINEAR)
    g.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON)

    var top = 0
    val chArr = Array[Char](0)
    val fm = g.getFontMetrics
    val w = getWidth

    for (opmeta <- historyText) {
      var height = top + 20
      var cx = 0

      for (ch <- opmeta.text) {
        if (ch == '\n') {
          cx = 0
          height += 20
        } else {
          val chw = fm.charWidth(ch)
          if (cx + chw > w - 10) {
            cx = 0
            height += 18
          }
          chArr(0) = ch
          g.drawChars(chArr, 0, 1, cx + 5, height)
          cx += chw
        }
      }

      top += opmeta.height
    }
  }

  class HistoryMeta(val text: String, val height: Int, val history: History)

}
