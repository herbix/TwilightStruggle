package me.herbix.ts.ui

import java.awt._
import javax.swing.JPanel

import me.herbix.ts.logic.Faction._
import me.herbix.ts.logic.{Region, Card, Country}
import me.herbix.ts.util.{MapValue, Resource}
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
  var flag = null

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

  override def paint(g: Graphics): Unit = {
    super.paint(g)

    val g2d = g.asInstanceOf[Graphics2D]
    g2d.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BILINEAR)
    g2d.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON)

    mode match {
      case DetailMode.CountryMode =>
        paintCountry(g2d)
      case _ =>
    }
  }

  val stroke1 = new BasicStroke(2)
  val stroke2 = new BasicStroke(2, BasicStroke.CAP_SQUARE, BasicStroke.JOIN_MITER, 10, Array(4, 3), 0)
  val stroke3 = new BasicStroke(1)

  def paintCountry(g: Graphics2D): Unit = {
    val colorTitle = if (country.critical) Resource.countryCriticalTitleColor else Resource.countryTitleColor
    val colorStability = if (country.critical) Resource.countryCriticalStabilityColor else Resource.countryStabilityColor
    val colorText = if (country.critical) Resource.countryCriticalTextColor else Resource.countryTextColor
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
      g.fillPolygon(Array(0, 180, 180), Array(120, 30, 120), 3)
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
    val str1 = country.toString
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

}
