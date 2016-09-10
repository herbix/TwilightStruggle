package me.herbix.ts.ui

import java.awt.event._
import java.awt.{Color, Graphics, Graphics2D, RenderingHints}
import javax.swing.{JButton, JPanel}

import me.herbix.ts.logic.Faction._
import me.herbix.ts.logic._
import me.herbix.ts.logic.card.{Cards, Card}
import me.herbix.ts.util.{Lang, Resource}

import scala.collection.mutable

/**
  * Created by Chaofan on 2016/6/14.
  */
class HandUI(val game: Game) extends JPanel with ActionListener {

  game.stateUpdateListeners :+= (() => {
    if (hasEventCards && game.operatingPlayer == game.playerFaction) {
      if (!eventCards.isSelected) {
        switchToButton(eventCards)
      }
    } else if (eventCards.isSelected) {
      switchToButton(selfHand)
    }

    if (game.isSpectator) {
      selfHand.setText(Lang.usHand)
      otherHand.setText(Lang.ussrHand)
    }

    updateState()
  })

  val cardpos = new Array[Int](120)
  val cards = new Array[Card](120)
  val cardsEnabled = new Array[Boolean](120)
  var hoverCardId = -1

  val cardWidth = 150

  var pendingCardSelection: mutable.Set[Card] = null

  var cardHoverListeners: List[(Card) => Unit] = List()
  var cardClickListeners: List[(Card) => Unit] = List()

  setLayout(null)

  val selfHand = new JButton(Lang.selfHand)
  selfHand.setSelected(true)
  selfHand.setLocation(0, 5)
  selfHand.setSize(90, 35)
  selfHand.addActionListener(this)
  selfHand.setFocusable(false)
  add(selfHand)

  val otherHand = new JButton(Lang.oppositeHand)
  otherHand.setLocation(0, 40)
  otherHand.setSize(90, 35)
  otherHand.addActionListener(this)
  otherHand.setFocusable(false)
  add(otherHand)

  val discarded = new JButton(Lang.discardedCards)
  discarded.setLocation(0, 75)
  discarded.setSize(90, 35)
  discarded.addActionListener(this)
  discarded.setFocusable(false)
  add(discarded)

  val eventCards = new JButton(Lang.eventCards)
  eventCards.setLocation(0, 110)
  eventCards.setSize(90, 35)
  eventCards.addActionListener(this)
  eventCards.setFocusable(false)
  add(eventCards)


  override def paint(g: Graphics): Unit = {
    super.paint(g)

    g.asInstanceOf[Graphics2D].setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BILINEAR)

    val hand = getShowCardSet
    var n = 0

    if (hand == null) {
      return
    }

    g.setColor(Resource.textColor)

    for (card <- hand) {
      val l = cardpos(n)
      val img = if (shouldNotShowCards) Resource.card(0) else Resource.card(card.id)
      val t = if (n == hoverCardId || pendingCardSelection(cards(n))) 0 else 10
      g.drawImage(img, l, t, cardWidth, img.getHeight * cardWidth / img.getWidth, null)
      g.setColor(Resource.textColor)
      g.drawRoundRect(l, t, cardWidth, img.getHeight * cardWidth / img.getWidth, 5, 5)
      if (pendingCardSelection(cards(n))) {
        g.setColor(new Color(255, 80, 80, 90))
        g.fillRoundRect(l, t, cardWidth, img.getHeight * cardWidth / img.getWidth, 5, 5)
      }
      if (!cardsEnabled(n)) {
        g.setColor(new Color(90, 90, 90, 90))
        g.fillRoundRect(l, t, cardWidth, img.getHeight * cardWidth / img.getWidth, 5, 5)
      }
      n += 1
    }
  }

  def showOppositeHand = game.flags.hasFlag(Faction.getOpposite(game.playerFaction), Flags.AldrichAmes)

  addMouseMotionListener(new MouseMotionAdapter {
    override def mouseMoved(e: MouseEvent): Unit = {
      val x = e.getX
      val y = e.getY
      val oldHoverCard = hoverCardId
      hoverCardId = -1
      if (!(y >= 0 && y < 160 && x >= 10 && x < getWidth)) {
        return
      }
      for (i <- cardpos.indices) {
        if (cardpos(i) <= x && cardpos(i) + cardWidth > x) {
          hoverCardId = i
        }
      }
      if (oldHoverCard != hoverCardId) {
        if (hoverCardId != -1) {
          cardHoverListeners.foreach(_(cards(hoverCardId)))
        }
        updateCardPos()
        repaint()
      }
    }
  })

  addMouseListener(new MouseListener {
    override def mouseExited(e: MouseEvent): Unit = {
      hoverCardId = -1
      updateCardPos()
      repaint()
    }
    override def mouseClicked(e: MouseEvent): Unit = {
      if (hoverCardId != -1 && cardsEnabled(hoverCardId)) {
        cardClickListeners.foreach(_(cards(hoverCardId)))
      }
    }
    override def mouseEntered(e: MouseEvent): Unit = {}
    override def mousePressed(e: MouseEvent): Unit = {}
    override def mouseReleased(e: MouseEvent): Unit = {}
  })

  addComponentListener(new ComponentAdapter {
    override def componentResized(e: ComponentEvent): Unit = {
      updateCardPos()
      repaint()
    }
  })

  def getCardEnabled(card: Card): Boolean = {
    if ((selfHand.isSelected && !hasEventCards) || (eventCards.isSelected && hasEventCards)) {
      game.getOperationHint match {
        case oh: OperationSelectCardAndActionHint => oh.canPlay(game, card)
        case oh: OperationSelectCardHint => oh.valid(game, card)
        case oh: OperationSelectCardsHint => oh.valid(game, card)
        case _ => true
      }
    } else {
      true
    }
  }

  def updateCardPos(): Unit = {

    val hand = getShowCardSet

    if (hand == null) {
      for (i <- cardpos.indices) {
        cardpos(i) = Integer.MAX_VALUE
        cards(i) = null
      }
      return
    }

    val w = Math.min(getWidth - cardWidth - 101, 120 * (hand.cardCount - 1))
    val c = hand.cardCount - 1
    var n = 0

    for (card <- hand) {
      cardpos(n) = if (c == 0)
        100
      else if (hoverCardId == c || hoverCardId == -1 || c == 1 || w / c > cardWidth)
        100 + w * n / c
      else if (n <= hoverCardId)
        100 + (w - cardWidth) * n / (c - 1)
      else
        100 + (w - cardWidth) * (n - 1) / (c - 1) + cardWidth
      cards(n) = if (shouldNotShowCards) Cards.fromId(0) else card
      cardsEnabled(n) = getCardEnabled(card)
      n += 1
    }

    for (i <- n until cardpos.length) {
      cardpos(i) = Integer.MAX_VALUE
      cards(i) = null
    }
  }

  def shouldNotShowCards: Boolean = !game.isSpectator && otherHand.isSelected && !showOppositeHand

  def hasEventCards: Boolean = {
    val data = game.currentCardData
    data != null && data.isInstanceOf[CardSet]
  }

  def getShowCardSet: CardSet = {
    if (game.playerFaction == Neutral && !discarded.isSelected) {
      if (game.isSpectator) {
        if (selfHand.isSelected) {
          game.hand(US)
        } else {
          game.hand(USSR)
        }
      } else {
        null
      }
    } else if (selfHand.isSelected) {
      game.hand(game.playerFaction)
    } else if (otherHand.isSelected) {
      game.hand(getOpposite(game.playerFaction))
    } else if (discarded.isSelected) {
      game.discards
    } else if (eventCards.isSelected && game.operatingPlayer == game.playerFaction) {
      if (hasEventCards) {
        game.currentCardData.asInstanceOf[CardSet]
      } else {
        null
      }
    } else {
      null
    }
  }

  def updateState(): Unit = {
    updateCardPos()
    repaint()
  }

  override def actionPerformed(e: ActionEvent): Unit = {
    switchToButton(e.getSource.asInstanceOf[JButton])
  }

  def switchToButton(button: JButton): Unit = {
    selfHand.setSelected(false)
    otherHand.setSelected(false)
    discarded.setSelected(false)
    eventCards.setSelected(false)
    button.setSelected(true)
    updateState()
  }
}
