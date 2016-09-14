package me.herbix.ts.ui

import java.awt._
import java.awt.event.{MouseEvent, MouseListener, MouseMotionAdapter}
import javax.swing.JPanel

import me.herbix.ts.logic._
import me.herbix.ts.logic.card.{Card016WarsawPact, Card020OlympicGames, Card067GrainSales}
import me.herbix.ts.util.{Lang, Resource}

import scala.collection.immutable.List
import scala.collection.mutable

/**
  * Created by Chaofan on 2016/6/17.
  */
class HistoryUI(g: Game) extends JPanel {

  val game = g.asInstanceOf[GameRecordingHistory]

  game.stateUpdateListeners :+= (() => updateContent())

  var historyText: mutable.Stack[HistoryMeta] = mutable.Stack()

  var hoverHistory: History = null

  var historyHoverListeners: List[(History) => Unit] = List.empty
  var historyClickListeners: List[(History) => Unit] = List.empty

  def updateContent(): Unit = {
    if (historyText.nonEmpty) historyText.pop()
    while (historyText.nonEmpty && !historyText.top.history.isInstanceOf[HistoryTurnRound]) {
      historyText.pop()
    }
    if (historyText.isEmpty) {
      historyText.pushAll(game.history.toStream.reverse.map(getHistoryMeta))
    } else {
      val myNewest = historyText.top.history
      historyText.pushAll(game.history.toStream.slice(0, game.history.indexOf(myNewest)).reverse.map(getHistoryMeta))
    }

    setPreferredSize(new Dimension(10, 1 + historyText.iterator.map(_.height).sum))
    getParent.revalidate()

    repaint()
  }

  def getHistoryMeta(history: History): HistoryMeta = {
    val str: String = history match {
      case h: HistoryStartGame => Lang.historyStartGame
      case h: HistoryTurnRound =>
        val str = if (h.round == 0) {
          String.format(Lang.historyTurnHeadline, h.turn.toString)
        } else if (h.round == -1) {
          String.format(Lang.historyTurnStart, h.turn.toString)
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
      case h: HistoryPickCard =>
        String.format(Lang.historyPickCard, Lang.getFactionName(h.faction), h.count.toString)
      case h: HistoryGetCard =>
        String.format(Lang.historyGetCard, Lang.getFactionName(h.faction), Lang.cardInfo(h.card.id)._1)
      case h: HistoryDiscardCard =>
        String.format(Lang.historyDiscardCard, Lang.getFactionName(h.faction), Lang.cardInfo(h.card.id)._1)
      case h: HistoryLoseCard =>
        String.format(Lang.historyLoseCard, Lang.getFactionName(h.faction), Lang.cardInfo(h.card.id)._1)
      case h: HistoryModifyInfluence =>
        val sb = new StringBuilder
        val change = if (h.isAdd) Lang.increase else Lang.decrease
        sb.append(String.format(Lang.historyModifyInfluence, Lang.getFactionName(h.faction), change))
        sb.append('\n')
        for ((country, oldValue, newValue) <- h.detail) {
          sb.append(String.format(Lang.historyModifyInfluenceDetail, Lang.countryNames(country.name), oldValue.toString, newValue.toString))
          sb.append('\n')
        }
        sb.length -= 1
        sb.toString
      case h: HistoryPlayHeadline =>
        String.format(Lang.historyPlayHeadline, Lang.getFactionName(h.faction), Lang.cardInfo(h.card.id)._1)
      case h: HistoryEvent =>
        val faction = if (h.card.faction != Faction.Neutral) h.card.faction else h.faction
        String.format(Lang.historyEvent, Lang.getFactionName(faction), Lang.cardInfo(h.card.id)._1)
      case h: HistoryCardAction =>
        if (!h.oppositeCard || h.action == Action.Space) {
          String.format(Lang.historyCardAction, Lang.getFactionName(h.faction), Lang.cardInfo(h.card.id)._1, Lang.getActionName(h.action))
        } else {
          val otherAction = if (h.action == Action.Event) Action.Operation else Action.Event
          String.format(Lang.historyCardActionOpposite, Lang.getFactionName(h.faction), Lang.cardInfo(h.card.id)._1, Lang.getActionName(h.action), Lang.getActionName(otherAction))
        }
      case h: HistoryCardOperation =>
        String.format(Lang.historyCardOperation, Lang.getFactionName(h.faction), Lang.cardInfo(h.card.id)._1, h.card.op.toString, Lang.getActionName(h.operation))
      case h: HistoryOperationSpace =>
        String.format(Lang.historyOperationSpace, Lang.getFactionName(h.faction), h.roll.toString)
      case h: HistoryOperationRealignment =>
        String.format(Lang.historyOperationRealignment, Lang.countryNames(h.country.name),
          h.usDice.toString, h.modifiedUsDice.toString,
          h.ussrDice.toString, h.modifiedUssrDice.toString)
      case h: HistoryOperationCoup =>
        String.format(Lang.historyOperationCoup, Lang.getFactionName(h.faction), Lang.countryNames(h.country.name),
          h.dice.toString, h.modifier.toString, h.result.toString)
      case h: HistorySpace =>
        String.format(Lang.historySpace, Lang.getFactionName(h.faction), h.oldLevel.toString, h.newLevel.toString)
      case h: HistoryVp =>
        String.format(Lang.historyVp, Lang.getFactionName(h.faction), h.vpChange.toString)
      case h: HistoryDefcon =>
        val formatStr = if (h.oldValue < h.newValue) {
          Lang.historyDefconImprove
        } else if (h.oldValue > h.newValue) {
          Lang.historyDefconDegrade
        } else {
          Lang.historyDefconStay
        }
        String.format(formatStr, h.oldValue.toString, h.newValue.toString)
      case h: HistoryMilitary =>
        if (h.newValue == 0) {
          String.format(Lang.historyMilitaryReset, Lang.getFactionName(h.faction))
        } else {
          String.format(Lang.historyMilitary, Lang.getFactionName(h.faction), h.oldValue.toString, h.newValue.toString)
        }
      case h: HistoryScoring =>
        String.format(Lang.historyScoring, Lang.getRegionName(h.region),
          h.usAll.toString, h.usBattle.toString, h.ussrAll.toString, h.ussrBattle.toString)
      case h: HistoryWar =>
        String.format(Lang.historyWar, Lang.getFactionName(h.faction), Lang.countryNames(h.country.name),
          h.dice.toString, h.result.toString)
      case h: HistoryRollDice =>
        if (h.modifier == 0) {
          String.format(Lang.historyRollDice, Lang.getFactionName(h.faction), h.dice.toString)
        } else {
          String.format(Lang.historyRollDiceModified, Lang.getFactionName(h.faction), h.dice.toString, (h.dice + h.modifier).toString)
        }
      case h: HistoryPokeChest =>
        String.format(Lang.historyPokeChest, Lang.getFactionName(h.faction), Lang.getFactionName(Faction.getOpposite(h.faction)))
      case h: HistoryAddFlag =>
        if (h.faction == Faction.Neutral) {
          String.format(Lang.historyAddFlagNeutral, Lang.flagInfo(h.flag.id)._1(h.faction))
        } else {
          String.format(Lang.historyAddFlag, Lang.getFactionName(h.faction), Lang.flagInfo(h.flag.id)._1(h.faction))
        }
      case h: HistoryRemoveFlag =>
        if (h.faction == Faction.Neutral) {
          String.format(Lang.historyRemoveFlagNeutral, Lang.flagInfo(h.flag.id)._1(h.faction))
        } else {
          String.format(Lang.historyRemoveFlag, Lang.getFactionName(h.faction), Lang.flagInfo(h.flag.id)._1(h.faction))
        }
      case h: HistoryRegion =>
        String.format(Lang.historyRegion, Lang.getFactionName(h.faction), Lang.getRegionName(h.region))
      case h: HistoryYesNo =>
        val value = h.fromCard match {
          case Card016WarsawPact => if(h.value) Lang.yes else Lang.no
          case Card020OlympicGames => if(h.value) Lang.yes else Lang.no
          case Card067GrainSales => if(h.value) Lang.play else Lang.giveBack
          case _ => if(h.value) Lang.yes else Lang.no
        }
        String.format(Lang.historyYesNo, Lang.getFactionName(h.faction), value)
      case h => h.toString
    }
    var height = 25

    var hasButton = false
    val snapshot = history.snapshot
    if (snapshot != null && !snapshot.needApproval && snapshot.operatingPlayer == game.playerFaction) {
      hasButton = true
    }

    val g = getGraphics
    val fm = g.getFontMetrics
    val w = getWidth
    var cx = 0

    for (ch <- str) {
      if (ch == '\n') {
        cx = 0
        height += 18
        hasButton = false
      } else {
        val chw = fm.charWidth(ch)
        val rw = if (hasButton) w - 17 else w
        if (cx + chw > rw - 10) {
          cx = 0
          height += 16
          hasButton = false
        }
        cx += chw
      }
    }

    new HistoryMeta(str, height, history)
  }

  val hoverColor = new Color(255, 255, 255, 128)

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

    for (historyMeta <- historyText) {
      historyMeta.history match {
        case h: HistoryStartGame =>
          g.setColor(Color.LIGHT_GRAY)
          g.fillRect(0, top, w, historyMeta.height)
        case h: HistoryTurnRound =>
          h.faction match {
            case Faction.Neutral => g.setColor(Color.LIGHT_GRAY)
            case Faction.US => g.setColor(Resource.usColorInfluenceChange)
            case Faction.USSR => g.setColor(Resource.ussrColorInfluenceChange)
          }
          g.fillRect(0, top, w, historyMeta.height)
        case _ =>
      }

      if (hoverHistory == historyMeta.history) {
        g.setColor(hoverColor)
        g.fillRect(0, top, w, historyMeta.height)
      }

      var hasButton = false
      val snapshot = historyMeta.history.snapshot
      if (snapshot != null && !snapshot.needApproval && snapshot.operatingPlayer == game.playerFaction) {
        val buttonImg = if (hoverHistory == historyMeta.history) Resource.buttonCloseHover else Resource.buttonClose
        g.drawImage(buttonImg, w - 20, top + 3, null)
        hasButton = true
      }

      g.setColor(Color.GRAY)
      g.drawLine(0, top, w, top)

      g.setColor(Color.BLACK)

      var height = top + 17
      var cx = 0

      for (ch <- historyMeta.text) {
        if (ch == '\n') {
          cx = 0
          height += 18
          hasButton = false
        } else {
          val chw = fm.charWidth(ch)
          val rw = if (hasButton) w - 17 else w
          if (cx + chw > rw - 10) {
            cx = 0
            height += 16
            hasButton = false
          }
          chArr(0) = ch
          g.drawChars(chArr, 0, 1, cx + 5, height)
          cx += chw
        }
      }

      top += historyMeta.height
    }

    g.setColor(Color.GRAY)
    g.drawLine(0, top, w, top)
  }

  addMouseMotionListener(new MouseMotionAdapter {
    override def mouseMoved(e: MouseEvent): Unit = {
      val y = e.getY
      var top = 0
      val meta = historyText.find(meta => {
        val r = y >= top && y < top + meta.height
        top += meta.height
        r
      }).orNull
      val history = if (meta == null) null else meta.history
      if (hoverHistory != history) {
        hoverHistory = history
        historyHoverListeners.foreach(_(hoverHistory))
        repaint()
      }
    }
  })

  addMouseListener(new MouseListener {
    override def mouseExited(e: MouseEvent): Unit = {
      hoverHistory = null
      historyHoverListeners.foreach(_(hoverHistory))
      repaint()
    }
    override def mouseClicked(e: MouseEvent): Unit = {
      if (hoverHistory != null) {
        historyClickListeners.foreach(_(hoverHistory))
      }
    }
    override def mouseEntered(e: MouseEvent): Unit = {}
    override def mousePressed(e: MouseEvent): Unit = {}
    override def mouseReleased(e: MouseEvent): Unit = {}
  })

  class HistoryMeta(val text: String, val height: Int, val history: History)

}
