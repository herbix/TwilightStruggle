package me.herbix.ts.ui

import java.awt._
import java.awt.event._
import java.awt.image.BufferedImage
import javax.swing.{JPanel, JScrollPane, SwingUtilities}

import me.herbix.ts.logic.{Country, Faction, Game}
import me.herbix.ts.util.MapValue
import me.herbix.ts.util.Resource._

import scala.List
import scala.collection.mutable

/**
  * Created by Chaofan on 2016/6/14.
  */
class WorldMapUI(val game: Game) extends JPanel {

  game.stateUpdateListeners :+= (() => repaint())

  var outerView: JScrollPane = null

  def setOuter(outer: JScrollPane) = {
    outerView = outer
    outerView.addComponentListener(new ComponentAdapter {
      override def componentResized(e: ComponentEvent): Unit = {
        minscale = Math.max((outerView.getWidth - 2).toDouble / bg.getWidth, (outerView.getHeight - 2).toDouble / bg.getHeight)
        adjustScale()
      }
    })
  }

  var minscale = 0.3
  var scale = minscale

  val bg = worldMap

  var pendingInfluenceChange: mutable.Map[Country, Int] = null
  var pendingInfluenceFaction = Faction.US
  var pendingInfluenceIsAdd = true

  var pendingCountrySelection: mutable.Set[Country] = null

  var countryHoverListeners: List[Country => Unit] = List()
  var countryClickListeners: List[(Country, Int) => Unit] = List()

  setPreferredSize(new Dimension((bg.getWidth * scale).toInt, (bg.getHeight * scale).toInt))

  addMouseWheelListener(new MouseWheelListener {
    override def mouseWheelMoved(e: MouseWheelEvent): Unit = {
      val x = e.getX
      val y = e.getY
      val x2 = x - outerView.getHorizontalScrollBar.getValue
      val y2 = y - outerView.getVerticalScrollBar.getValue
      val oldScale = scale

      scale *= Math.pow(1.1, -e.getWheelRotation)
      adjustScale()

      SwingUtilities.invokeLater(new Runnable {
        override def run(): Unit = {
          outerView.getHorizontalScrollBar.setValue((x * scale / oldScale).toInt - x2)
          outerView.getVerticalScrollBar.setValue((y * scale / oldScale).toInt - y2)
        }
      })
    }
  })

  def adjustScale(): Unit = {
    if (scale > 1) {
      scale = 1
    } else if (scale < minscale) {
      scale = minscale
    }
    setPreferredSize(new Dimension((bg.getWidth * scale).toInt, (bg.getHeight * scale).toInt))

    getParent.revalidate()
  }

  private val mouseAdapter: MouseAdapter = new MouseAdapter {
    var oldx = 0
    var oldy = 0
    var dragging = false
    var oldHoverCountry: Country = null
    override def mousePressed(e: MouseEvent): Unit = {
      if (e.getButton != MouseEvent.BUTTON1) {
        dragging = true
      }
      oldx = e.getX - outerView.getHorizontalScrollBar.getValue
      oldy = e.getY - outerView.getVerticalScrollBar.getValue
    }
    override def mouseReleased(e: MouseEvent): Unit = {
      dragging = false
    }
    override def mouseClicked(e: MouseEvent): Unit = {
      val country = findCountry(e.getX, e.getY)
      if (country != oldHoverCountry) {
        oldHoverCountry = country
      }
      if (country != null) {
        countryClickListeners.foreach(_(country, e.getButton))
      }
    }
    override def mouseMoved(e: MouseEvent): Unit = {
      val country = findCountry(e.getX, e.getY)
      if (country != oldHoverCountry) {
        oldHoverCountry = country
        if (country != null) {
          countryHoverListeners.foreach(_(country))
        }
      }
    }
    override def mouseDragged(e: MouseEvent): Unit = {
      if (dragging) {
        val x = e.getX - outerView.getHorizontalScrollBar.getValue
        val y = e.getY - outerView.getVerticalScrollBar.getValue
        outerView.getHorizontalScrollBar.setValue(outerView.getHorizontalScrollBar.getValue - x + oldx)
        outerView.getVerticalScrollBar.setValue(outerView.getVerticalScrollBar.getValue - y + oldy)
        oldx = x
        oldy = y
      }
    }
    def findCountry(x: Int, y: Int): Country =
      MapValue.countryPosMap.find(e => {
        val nx = x / scale
        val ny = y / scale
        val cx = e._2._1
        val cy = e._2._2
        nx >= cx && nx < cx + 101 && ny >= cy && ny < cy + 67
      }) match {
        case Some(e) => game.worldMap.countries(e._1)
        case None => null
      }
  }

  addMouseListener(mouseAdapter)
  addMouseMotionListener(mouseAdapter)

  override def paint(g: Graphics): Unit = {
    super.paint(g)

    val g2d = g.asInstanceOf[Graphics2D]

    g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    g2d.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BILINEAR)
    g2d.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON)
    g2d.scale(scale, scale)
    g.drawImage(bg, 0, 0, null)

    drawTurnToken(g)
    drawDefconToken(g)
    drawVPToken(g)

    import me.herbix.ts.logic.Faction._

    val round = if (game.round < 1) 1 else game.round
    val isUS = game.phasingPlayer == US && game.round >= 1
    val roundColor = if (isUS) usColor else ussrColor
    val roundImg = if (isUS) tokenActionUs else tokenActionUssr
    drawActionToken(g, roundColor, roundImg, round)

    drawMilitaryToken(g, usColor, game.military(US), -tokenSize/2, tokenMilitaryUs)
    drawMilitaryToken(g, ussrColor, game.military(USSR), tokenSize/2, tokenMilitaryUssr)

    drawSpaceToken(g, usColor, game.space(US).level, -tokenSize/2, tokenSpaceUs)
    drawSpaceToken(g, ussrColor, game.space(USSR).level, tokenSize/2, tokenSpaceUssr)

    g.setFont(influenceTokenTextFont)
    val fm = g.getFontMetrics

    for ((name, country) <- game.worldMap.normalCountries) {
      val influenceChanged = pendingInfluenceChange.contains(country)
      val usInfluenceChanged = influenceChanged && pendingInfluenceFaction == US
      val ussrInfluenceChanged = influenceChanged && pendingInfluenceFaction == USSR
      val factor = if (pendingInfluenceIsAdd) 1 else -1

      val usInfluence = country.influence(US) + factor *
        (if (usInfluenceChanged) pendingInfluenceChange(country) else 0)
      val ussrInfluence = country.influence(USSR) + factor *
        (if (ussrInfluenceChanged) pendingInfluenceChange(country) else 0)

      val usDrawColor = if (usInfluenceChanged) usColorInfluenceChange else usColor
      val usBgColor = if (usInfluenceChanged) new Color(255, 255, 255, 128) else Color.WHITE
      val ussrDrawColor = if (ussrInfluenceChanged) ussrColorInfluenceChange else ussrColor
      val ussrBgColor = if (ussrInfluenceChanged) new Color(255, 255, 255, 128) else Color.WHITE

      val (x, y) = MapValue.countryPosMap(name)

      if (usInfluence > 0) {
        if (usInfluence - ussrInfluence >= country.stability) {
          drawInfluenceToken(g, fm, usInfluence.toString, usDrawColor, usBgColor, x + 2, y + 18)
        } else {
          drawInfluenceToken(g, fm, usInfluence.toString, usBgColor, usDrawColor, x + 2, y + 18)
        }
      }
      if (ussrInfluence > 0) {
        if (ussrInfluence - usInfluence >= country.stability) {
          drawInfluenceToken(g, fm, ussrInfluence.toString, ussrDrawColor, ussrBgColor, x + 52, y + 18)
        } else {
          drawInfluenceToken(g, fm, ussrInfluence.toString, ussrBgColor, ussrDrawColor, x + 52, y + 18)
        }
      }

      if (pendingCountrySelection(country)) {
        g.setColor(selectedCountry)
        g.fillRect(x, y, MapValue.countrySize._1, MapValue.countrySize._2)
      }
    }
  }

  def drawInfluenceToken(g: Graphics, fm: FontMetrics, influenceStr: String, bgColor: Color, frontColor: Color, mx: Int, my: Int): Unit = {
    drawToken(g, bgColor, mx, my)
    g.setColor(frontColor)
    g.drawString(influenceStr, mx + 23 - fm.stringWidth(influenceStr) / 2, my + 23 + textFont.getSize / 2 + 3)
  }

  def drawToken(g: Graphics, bgColor: Color, mx: Int, my: Int): Unit = {
    g.setColor(bgColor)
    g.fillRoundRect(mx, my, tokenSize, tokenSize, 10, 10)
    g.setColor(Color.DARK_GRAY)
    g.drawRoundRect(mx, my, tokenSize, tokenSize, 10, 10)
  }

  def getTokenCenterX(xStart: Int, xEnd: Int, start: Int, end: Int, value: Int): Int = {
    ((end - value) * xStart + (value - start) * xEnd) / (end - start)
  }

  def getTokenCenter(rect: (Int, Int, Int, Int), offset: Int, count: Int, value: Int): (Int, Int) = {
    val halfToken: Int = ((rect._3 - rect._1) / count) / 2
    val xStart = halfToken + rect._1
    val xEnd = rect._3 - halfToken
    val y = (rect._2 + rect._4) / 2
    val x = getTokenCenterX(xStart, xEnd, offset, offset + count - 1, value)
    (x, y)
  }

  def getTokenCenter(startPos: (Int, Int), endPos: (Int, Int), size: (Int, Int),
                     start: Int, end: Int, value: Int): (Int, Int) = {
    val xStart = startPos._1 + size._1 / 2
    val xEnd = endPos._1 + size._1 / 2
    val y = startPos._2 + size._2 / 2
    val x = getTokenCenterX(xStart, xEnd, start, end, value)
    (x, y)
  }

  def drawTokenString(g: Graphics, x: Int, y: Int, turn: String): Unit = {
    g.setColor(Color.BLACK)
    val fm = g.getFontMetrics
    g.drawString(turn, x - fm.stringWidth(turn) / 2, y + fm.getHeight / 2 - 4)
  }

  def drawTurnToken(g: Graphics): Unit = {
    val (x, y) = getTokenCenter(MapValue.turnArea, 1, 10, game.turn)
    drawToken(g, tokenColor, x - tokenSize / 2, y - tokenSize / 2)
    g.setFont(tokenFont)
    drawTokenString(g, x, y, "Turn")
  }

  def drawDefconToken(g: Graphics): Unit = {
    val (x, y) = getTokenCenter(MapValue.defcon1, MapValue.defcon5, MapValue.defconSize, 1, 5, game.defcon)
    val img = tokenDefcon
    drawToken(g, tokenColor, x - tokenSize / 2, y - tokenSize / 2)
    g.drawImage(img, x - img.getWidth / 2, y - img.getHeight / 2, null)
  }

  def drawVPToken(g: Graphics): Unit = {
    val (x, y) = getTokenCenter(MapValue.vpUs20, MapValue.vpUssr20, MapValue.vpSize, 20, -20, game.vp)
    drawToken(g, tokenColor, x - tokenSize / 2, y - tokenSize / 2)
    g.setFont(tokenFont2)
    drawTokenString(g, x, y, "VP")
  }

  def drawActionToken(g: Graphics, bgColor: Color, img: BufferedImage, value: Int): Unit = {
    val (x, y) = getTokenCenter(MapValue.round1, MapValue.round8, MapValue.roundSize, 1, 8, value)
    drawToken(g, bgColor, x - tokenSize / 2, y - tokenSize / 2)
    g.drawImage(img, x - img.getWidth / 2, y - img.getHeight / 2, null)
  }

  def drawSpaceToken(g: Graphics, bgColor: Color, level: Int, yOffset: Int, img: BufferedImage): Unit = {
    val (x, y) = getTokenCenter(MapValue.space0, MapValue.space8, MapValue.spaceSize, 0, 8, level)
    drawToken(g, bgColor, x - tokenSize / 2, y - tokenSize / 2 + yOffset)
    g.drawImage(img, x - img.getWidth / 2, y - img.getHeight / 2 + yOffset, null)
  }

  def drawMilitaryToken(g: Graphics, bgColor: Color, level: Int, yOffset: Int, img: BufferedImage): Unit = {
    val (x, y) = getTokenCenter(MapValue.military0, MapValue.military5, MapValue.militarySize, 0, 5, level)
    drawToken(g, bgColor, x - tokenSize / 2, y - tokenSize / 2 + yOffset)
    g.drawImage(img, x - img.getWidth / 2, y - img.getHeight / 2 + yOffset, null)
  }
}
