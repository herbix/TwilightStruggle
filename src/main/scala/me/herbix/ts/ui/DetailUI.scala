package me.herbix.ts.ui

import java.awt._
import javax.swing.JPanel

import me.herbix.ts.logic.Faction._
import me.herbix.ts.logic._
import me.herbix.ts.util.{Lang, MapValue, Resource}
import me.herbix.ts.util.Resource._

/**
  * Created by Chaofan on 2016/6/17.
  */
class DetailUI extends JPanel {

  object DetailMode extends Enumeration {
    type DetailMode = Value
    val CountryMode, CardMode, FlagMode, EmptyMode = Value
  }

  import DetailMode._

  var mode = EmptyMode
  var country: Country = null
  var card: Card = null
  var flag: Flag = null
  var flagFaction: Faction = US

  def setCountry(country: Country): Unit = {
    mode = CountryMode
    this.country = country
    repaint()
  }

  def setCard(card: Card): Unit = {
    mode = CardMode
    this.card = card
    repaint()
  }

  def setFlag(faction: Faction, flag: Flag): Unit = {
    mode = FlagMode
    this.flag = flag
    this.flagFaction = faction
    repaint()
  }

  override def paint(g: Graphics): Unit = {
    super.paint(g)

    val g2d = g.asInstanceOf[Graphics2D]
    g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    g2d.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BILINEAR)
    g2d.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON)

    mode match {
      case DetailMode.CountryMode =>
        paintCountry(g2d)
      case DetailMode.CardMode =>
        paintCard(g2d)
      case DetailMode.FlagMode =>
        paintFlag(g2d)
      case _ =>
    }
  }

  val stroke1 = new BasicStroke(2)
  val stroke2 = new BasicStroke(2, BasicStroke.CAP_SQUARE, BasicStroke.JOIN_MITER, 10, Array(4, 3), 0)
  val stroke3 = new BasicStroke(1)
  val countryTriangleX = Array(0, 180, 180)
  val countryTriangleY = Array(120, 30, 120)

  def paintCountry(g: Graphics2D): Unit = {
    val colorTitle = if (country.isBattlefield) Resource.countryCriticalTitleColor else Resource.countryTitleColor
    val colorStability = if (country.isBattlefield) Resource.countryCriticalStabilityColor else Resource.countryStabilityColor
    val colorText = if (country.isBattlefield) Resource.countryCriticalTextColor else Resource.countryTextColor
    val (color1, color2) = {
      val colorRegions = country.regions.filter(Resource.regionColor.contains)
      if (colorRegions.size == 1) {
        (Resource.regionColor(colorRegions.head), null)
      } else if (colorRegions.contains(Region.Asia)){
        (Resource.regionColor(Region.Asia), Resource.regionColor(Region.SouthEastAsia))
      } else if (colorRegions.contains(Region.WestEurope)) {
        (Resource.regionColor(Region.WestEurope), Resource.regionColor(Region.EastEurope))
      } else {
        (null, null)
      }
    }

    g.translate(10, 20)

    g.setColor(colorTitle)
    g.fillRect(0, 0, 150, 30)
    g.setColor(colorStability)
    g.fillRect(150, 0, 30, 30)
    g.setColor(color1)
    g.fillRect(0, 30, 180, 90)

    if (color2 != null) {
      g.setColor(color2)
      g.fillPolygon(countryTriangleX, countryTriangleY, 3)
    }

    g.setStroke(stroke1)
    g.setColor(Color.BLACK)
    g.drawRect(0, 0, 180, 120)
    g.drawLine(0, 30, 180, 30)
    g.drawLine(150, 0, 150, 30)

    g.setStroke(stroke2)
    g.drawLine(90, 30, 90, 120)

    g.setStroke(stroke3)
    g.setFont(Resource.textFont2)
    g.setColor(colorText)
    val fm1 = g.getFontMetrics
    val str1 = Lang.countryNames(country.name)
    val w1 = fm1.stringWidth(str1)

    if (w1 > 145) {
      val trans = g.getTransform
      g.scale(145.0 / w1, 1)
      g.drawString(str1, 2.5f*w1/145, 25)
      g.setTransform(trans)
    } else {
      g.drawString(str1, (150-w1)/2, 25)
    }

    g.setFont(Resource.countryStabilityFont)
    val fm2 = g.getFontMetrics
    val str2 = country.stability.toString
    val w2 = fm2.stringWidth(str2)
    g.drawString(str2, 150+(30-w2)/2, 25)

    g.setFont(Resource.countryInfluenceFont)
    val fm = g.getFontMetrics

    val usInfluence = country.influence(US)
    val ussrInfluence = country.influence(USSR)
    val usDrawColor = usColor
    val usBgColor = Color.WHITE
    val ussrDrawColor = ussrColor
    val ussrBgColor = Color.WHITE

    if (usInfluence > 0) {
      if (usInfluence - ussrInfluence >= country.stability) {
        drawInfluenceToken(g, fm, usInfluence.toString, usDrawColor, usBgColor, 4, 34)
      } else {
        drawInfluenceToken(g, fm, usInfluence.toString, usBgColor, usDrawColor, 4, 34)
      }
    }
    if (ussrInfluence > 0) {
      if (ussrInfluence - usInfluence >= country.stability) {
        drawInfluenceToken(g, fm, ussrInfluence.toString, ussrDrawColor, ussrBgColor, 94, 34)
      } else {
        drawInfluenceToken(g, fm, ussrInfluence.toString, ussrBgColor, ussrDrawColor, 94, 34)
      }
    }
  }

  def drawInfluenceToken(g: Graphics2D, fm: FontMetrics, str: String, bgColor: Color, color: Color, x: Int, y: Int): Unit = {
    g.setColor(bgColor)
    g.fillRoundRect(x, y, 82, 82, 20, 20)
    g.setColor(Color.BLACK)
    g.drawRoundRect(x, y, 82, 82, 20, 20)
    g.setColor(color)
    g.drawString(str, x + (82 - fm.stringWidth(str)) / 2, y + 64)
  }

  val cardStarX = new Array[Int](10)
  val cardStarY = new Array[Int](10)
  val cardStarR = 20.0
  val cardStarR2 = cardStarR * Math.sin(Math.PI / 10) / Math.sin(Math.PI * 126 / 180)

  for (i <- 0 until 5) {
    cardStarX(i*2) = (-Math.sin(i * Math.PI * 2 / 5 - Math.PI / 5) * cardStarR2).toInt
    cardStarY(i*2) = -(Math.cos(i * Math.PI * 2 / 5 - Math.PI / 5) * cardStarR2).toInt
    cardStarX(i*2+1) = (-Math.sin(i * Math.PI * 2 / 5) * cardStarR).toInt
    cardStarY(i*2+1) = -(Math.cos(i * Math.PI * 2 / 5) * cardStarR).toInt
  }

  def paintCard(g: Graphics2D): Unit = {
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

    g.translate(10, 20)

    paintTitle(g, titleColor, titleText, card.faction, card.op)

    val name = Lang.cardInfo(card.id)._1
    val desc = Lang.cardInfo(card.id)._2

    paintName(g, name)
    paintDesc(g, desc)
  }

  def paintTitle(g: Graphics2D, titleColor: Color, titleText: String, faction: Faction, operationPoint: Int): Unit = {
    g.setColor(titleColor)
    g.fillRect(0, 0, 180, 17)
    g.setColor(Color.BLACK)
    g.drawRect(0, 0, 180, 17)

    g.setFont(Resource.cardTitleFont)
    g.setColor(Color.WHITE)
    val fm = g.getFontMetrics
    g.drawString(titleText, 38 + (142 - fm.stringWidth(titleText)) / 2, 15)

    val trans = g.getTransform
    g.translate(19, 9)
    g.setColor(Color.BLACK)
    g.fillPolygon(cardStarX, cardStarY, cardStarX.length)
    g.translate(-1, 0)
    faction match {
      case Faction.US =>
        g.setColor(Resource.cardStarUS2)
        g.fillPolygon(cardStarX, cardStarY, cardStarX.length)
        g.setColor(Resource.cardStarUS1)
        g.drawPolygon(cardStarX, cardStarY, cardStarX.length)
      case Faction.USSR =>
        g.setColor(Resource.cardStarUSSR2)
        g.fillPolygon(cardStarX, cardStarY, cardStarX.length)
        g.setColor(Resource.cardStarUSSR1)
        g.drawPolygon(cardStarX, cardStarY, cardStarX.length)
      case Faction.Neutral =>
        g.setColor(Resource.cardStarNeutral2)
        g.fillPolygon(cardStarX, cardStarY, cardStarX.length)
        g.setColor(Resource.cardStarNeutral1)
        g.fillPolygon(cardStarX, cardStarY, cardStarX.length / 2 + 1)
    }
    if (operationPoint > 0) {
      g.setFont(Resource.cardOpFont)
      g.setColor(if (faction == USSR) Color.WHITE else Color.BLACK)
      val str = operationPoint.toString
      val fm = g.getFontMetrics
      g.drawString(str, -fm.stringWidth(str) / 2, 7)
    }
    g.setTransform(trans)
  }

  def paintName(g: Graphics2D, name: String): Unit = {
    val trans = g.getTransform
    g.setColor(Color.BLACK)
    g.setFont(Resource.cardNameFont)
    val fm = g.getFontMetrics
    g.translate(90, 0)
    val strWidth = fm.stringWidth(name)
    if (strWidth > 180) {
      g.scale(180.0 / strWidth, 1)
    }
    g.drawString(name, -strWidth / 2, 50)
    g.setTransform(trans)
  }

  def paintDesc(g: Graphics2D, desc: String): Unit = {
    g.setFont(Resource.cardDescFont)
    g.setColor(Color.black)

    val fm = g.getFontMetrics

    val lines = desc.split("\n")
    var currentRow = 78
    var currentCol = 0
    val ca = Array[Char](0)

    for (line <- lines) {
      val toDraw = "  " + line
      for (i <- 0 until toDraw.length) {
        val c = toDraw.charAt(i)
        val cw = fm.charWidth(c)
        if (currentCol + cw > 180 && c != '，' && c != '。') {
          currentRow += 20
          currentCol = 0
        }
        ca(0) = c
        if (c == '~') {
          g.drawChars(ca, 0, 1, currentCol, currentRow + fm.getHeight / 2)
        } else {
          g.drawChars(ca, 0, 1, currentCol, currentRow)
        }
        currentCol += cw
      }
      currentRow += 25
      currentCol = 0
    }
  }

  def paintFlag(g: Graphics2D): Unit = {
    val titleColor = if (flag.isGoodFlag) Resource.cardTitleEarlyWar else Resource.cardTitleLateWar
    val titleText = if (flag.isGoodFlag) Lang.goodFlag else Lang.badFlag

    g.translate(10, 20)

    paintTitle(g, titleColor, titleText, flagFaction, 0)

    val name = Lang.flagInfo(flag.id)._1(flagFaction)
    val desc = if (flagFaction == Neutral)
      Lang.flagInfo(flag.id)._2
    else
      String.format(Lang.flagInfo(flag.id)._2, Lang.getFactionName(flagFaction))

    paintName(g, name)
    paintDesc(g, desc)
  }
}
