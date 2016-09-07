package me.herbix.ts.client

import java.awt._
import javax.swing._
import javax.swing.event.{ListSelectionEvent, ListSelectionListener}

import me.herbix.ts.logic.card.{Card, Cards}
import me.herbix.ts.util.{InfoItem, CardInfo, Lang, Resource}

/**
  * Created by Chaofan on 2016/9/5.
  */
object HelpDialog extends JFrame {

  setLayout(new BorderLayout())

  val helpListModel = new DefaultListModel[ListItem]
  val helpList = new JList[ListItem](helpListModel)
  val helpListOuter = new JScrollPane(helpList)

  helpList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION)
  helpListOuter.setPreferredSize(new Dimension(250, 600))

  add(helpListOuter, BorderLayout.WEST)

  val helpInfo = new JPanel {
    override def paint(g: Graphics): Unit = {
      super.paint(g)
      val g2d = g.asInstanceOf[Graphics2D]

      g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
      g2d.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BILINEAR)
      g2d.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON)

      paintContent(g2d, helpList.getSelectedValue, getWidth)
    }
  }
  val helpInfoOuter = new JScrollPane(helpInfo)

  val defaultFont = helpInfo.getFont
  helpInfo.setFont(new Font(defaultFont.getFontName, 0, (defaultFont.getSize * 1.3f).toInt))
  helpInfo.setPreferredSize(new Dimension(10, 10))
  helpInfoOuter.setPreferredSize(new Dimension(700, 650))

  add(helpInfoOuter)

  setTitle("帮助")

  pack()

  setLocationRelativeTo(getOwner)

  (1 to 110).map(Cards.fromId).foreach(card => helpListModel.addElement(new ListItem(card)))

  helpList.addListSelectionListener(new ListSelectionListener {
    override def valueChanged(e: ListSelectionEvent): Unit = {
      if (!e.getValueIsAdjusting) {
        val selected = helpList.getSelectedValue
        if (selected != null) {
          updateHelpInfoPreferredHeight(selected)
          helpInfo.repaint()
        }
      }
    }
  })

  class ListItem(val ref: Any) {
    override def toString: String = {
      ref match {
        case card: Card => f"${card.id}%03d ${Lang.cardInfo(card.id)._1}%s"
        case _ => ref.toString
      }
    }
  }

  def updateHelpInfoPreferredHeight(value: ListItem): Unit = {
    if (value == null) {
      return
    }

    value.ref match {
      case card: Card => helpInfo.setPreferredSize(new Dimension(20 + Resource.card(0).getWidth, 20 + Resource.card(0).getHeight))
      case _ =>
    }
  }

  def paintContent(g: Graphics2D, value: ListItem, width: Int): Unit = {
    if (value == null) {
      return
    }

    value.ref match {
      case card: Card => paintCardContent(g, card, width)
      case _ =>
    }
  }

  def paintCardContent(g: Graphics2D, card: Card, width: Int): Unit = {
    val cardImg = Resource.cardBg(card.id)

    g.translate((width - cardImg.getWidth) / 2, 10)
    g.drawImage(cardImg, 0, 0, null)

    val period = Cards.getCardPeriod(card)
    val titleColor = period match {
      case 2 => Resource.cardTitleMidWar
      case 3 => Resource.cardTitleLateWar
      case _ => Resource.cardTitleEarlyWar
    }
    val titleText = period match {
      case 2 => Lang.midWar
      case 3 => Lang.lateWar
      case _ => Lang.earlyWar
    }

    g.setColor(titleColor)
    g.fillRect(114, 55, 202, 22)

    g.setFont(new Font("隶书", 0, 24))
    val fm1 = g.getFontMetrics
    g.setColor(Color.WHITE)
    g.drawString(titleText, 200 - fm1.stringWidth(titleText) / 2, 75)

    for (info <- CardInfo.infoItems.find(_.name.toInt == card.id)) {
      if (info("start") != null) {
        val start = info("start").toInt

        if (info("background") == null) {
          g.setColor(Resource.cardBackground)
          g.fillRect(0, start - 5, cardImg.getWidth, cardImg.getHeight - start + 5)
        }

        val transform = g.getTransform
        val inWidth = (cardImg.getWidth * 0.8).toInt

        g.translate((cardImg.getWidth - inWidth) / 2, 0)
        paintName(g, card, info, inWidth, start + 20)
        val hasStar = info("star") != null
        val bottom = cardImg.getHeight - 10 - (if (hasStar) 15 else 0)
        paintDesc(g, card, info, inWidth, start + 55, bottom)

        if (hasStar) {
          g.setColor(Resource.cardRed)
          g.setFont(new Font("楷体", 0, 20))
          val fm = g.getFontMetrics
          g.drawString(Lang.removeFromGame, (inWidth - fm.stringWidth(Lang.removeFromGame)) / 2, cardImg.getHeight - 20)
        }

        g.setTransform(transform)
      } else if (info("scoring") != null) {
        val color1 = new Color(cardImg.getRGB(43, 127) | 0xFF000000)
        val color2 = new Color(cardImg.getRGB(335, 224) | 0xFF000000)
        val color3 = new Color(cardImg.getRGB(41, 230) | 0xFF000000)
        val color4 = new Color(cardImg.getRGB(41, 265) | 0xFF000000)
        val color5 = new Color(cardImg.getRGB(45, 464) | 0xFF000000)

        g.setColor(color1)
        g.fillRect(43, 127, 285, 97)
        g.setColor(color2)
        g.fillPolygon(Array[Int](328, 43, 332, 332), Array[Int](125, 225, 225, 125), 4)
        g.setColor(color3)
        g.fillRect(41, 230, 293, 30)
        g.setColor(color4)
        g.fillRect(41, 264, 293, 30)
        g.setColor(color3)
        g.fillRect(41, 297, 293, 30)
        g.setColor(color4)
        g.fillRect(41, 330, 293, 56)
        if (info.lines.length == 7) {
          g.setColor(color3)
          g.fillRect(41, 388, 293, 57)
        }
        g.setColor(color5)
        g.fillRect(44, 465, 287, 21)

        val transform = g.getTransform
        val inWidth = (cardImg.getWidth * 0.8).toInt

        g.translate((cardImg.getWidth - inWidth) / 2, 0)
        paintName(g, card, info, inWidth, 185)

        val content = info.lines.drop(1).dropRight(1)
        g.setFont(new Font("宋体", 0, 20))
        val fm = g.getFontMetrics

        if (content.length > 2) {
          g.drawString(content(0).content, (inWidth - fm.stringWidth(content(0).content)) / 2, 252)
          g.drawString(content(1).content, (inWidth - fm.stringWidth(content(1).content)) / 2, 252 + 33)
          g.drawString(content(2).content, (inWidth - fm.stringWidth(content(2).content)) / 2, 252 + 66)
          g.drawString(content(3).content, (inWidth - fm.stringWidth(content(3).content)) / 2, 365)
        } else {

        }

        if (content.length > 4) {
          val str = content(4).content
          val str1 = str.substring(0, 10)
          val str2 = str.substring(10)
          g.drawString(str1, (inWidth - fm.stringWidth(str1)) / 2, 410)
          g.drawString(str2, (inWidth - fm.stringWidth(str2)) / 2, 436)
        }

        g.setFont(new Font("黑体", 0, 20))
        g.setColor(new Color(0xfffef45e))

        val tip = info.lines.last.content
        g.drawString(tip, (inWidth - fm.stringWidth(tip)) / 2, 482)

        g.setTransform(transform)
      }
    }

    g.setColor(Color.DARK_GRAY)
    g.setStroke(new BasicStroke(1))
    g.drawRoundRect(0, 0, cardImg.getWidth, cardImg.getHeight, 10, 10)
  }

  def paintName(g: Graphics2D, card: Card, info: InfoItem, width: Int, start: Int): Unit = {
    val name = info(0).content
    val trans = g.getTransform

    g.setColor(Color.BLACK)
    g.setFont(new Font("隶书", 0, 36))
    val fm = g.getFontMetrics
    g.translate(width / 2, 0)
    val strWidth = fm.stringWidth(name)
    if (strWidth > width) {
      g.scale(width.toDouble / strWidth, 1)
    }
    val baseline = start
    g.drawString(name, -strWidth / 2, baseline)

    if (info("star") != null) {
      g.setFont(new Font("隶书", 0, 24))
      g.drawString("*", strWidth / 2, baseline - 7)
    }

    if (info("underline") != null) {
      g.setColor(Resource.cardRed)
      g.setStroke(new BasicStroke(2))
      g.drawLine(-strWidth / 2, baseline + 3, strWidth / 2, baseline + 3)
    }

    g.setTransform(trans)
  }

  def paintDesc(g: Graphics2D, card: Card, info: InfoItem, width: Int, start: Int, bottom: Int): Unit = {
    val transform = g.getTransform

    val expectedBottom = paintDescSim(g, card, info, width, 0)

    val desc = Lang.cardInfo(card.id)._2

    g.translate(0, start)

    if (expectedBottom > bottom - start) {
      g.scale(1, (bottom.toDouble - start) / expectedBottom)
    }

    g.setColor(Color.black)

    val lines = info.lines.drop(1)
    var currentRow = 0
    var currentCol = 0
    val ca = Array[Char](0)

    for (line <- lines) {
      if (line("italic") != null) {
        g.setFont(new Font("楷体", 0, 18))
      } else {
        g.setFont(new Font("宋体", 0, 20))
      }
      val fm = g.getFontMetrics

      val toDraw = "  " + line.content
      for (i <- 0 until toDraw.length) {
        val c = toDraw.charAt(i)
        val cw = fm.charWidth(c)
        if (currentCol + cw > width && c != '，' && c != '。') {
          currentRow += 23
          currentCol = 0
        }
        ca(0) = c
        if (c == '~') {
          g.drawChars(ca, 0, 1, currentCol, currentRow + fm.getHeight / 2 - 2)
        } else {
          g.drawChars(ca, 0, 1, currentCol, currentRow)
        }
        currentCol += cw
      }
      currentRow += 30
      currentCol = 0
    }

    g.setTransform(transform)
  }

  def paintDescSim(g: Graphics2D, card: Card, info: InfoItem, width: Int, start: Int): Int = {
    val desc = Lang.cardInfo(card.id)._2

    g.setFont(new Font("宋体", 0, 20))

    val fm = g.getFontMetrics

    val lines = desc.split("\n")
    var currentRow = start
    var currentCol = 0
    val ca = Array[Char](0)

    for (line <- lines) {
      val toDraw = "  " + line
      for (i <- 0 until toDraw.length) {
        val c = toDraw.charAt(i)
        val cw = fm.charWidth(c)
        if (currentCol + cw > width && c != '，' && c != '。') {
          currentRow += 23
          currentCol = 0
        }
        ca(0) = c
        currentCol += cw
      }
      currentRow += 28
      currentCol = 0
    }

    currentRow
  }

}
