// Copyright (C) 2017 Chaofan

package me.herbix.ts.ui

import java.awt._
import java.awt.event._
import java.awt.image.BufferedImage
import javax.swing.{Timer, JPanel, JScrollPane, SwingUtilities}

import me.herbix.ts.agent.simple.SimpleAgent
import me.herbix.ts.logic.Faction.Faction
import me.herbix.ts.logic.Region.Region
import me.herbix.ts.logic.SpaceLevel.SpaceLevel
import me.herbix.ts.logic._
import me.herbix.ts.logic.turnzero.{CrisisUnknown, Crisis, GameTurnZero, TZFlags}
import me.herbix.ts.util._
import me.herbix.ts.util.Resource._

import scala.List
import scala.collection.mutable

/**
  * Created by Chaofan on 2016/6/14.
  */
class WorldMapUI(g: Game) extends JPanel {

  val game = g.asInstanceOf[GameRecordingHistory]

  game.stateUpdateListeners :+= (() => {
    updateCrisisPositions()
    updateChangedModel()
    repaint()
  })

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

  var changedCountries: Set[Country] = Set.empty[Country]
  var changedCountriesFaction = Faction.US

  var availableCountries: Set[Country] = Set.empty[Country]

  var countryHoverListeners: List[Country => Unit] = List()
  var countryClickListeners: List[(Country, Int) => Unit] = List()

  var spaceHoverListeners: List[SpaceLevel => Unit] = List()

  var regionHoverListeners: List[Region => Unit] = List()

  val changedModel = new ChangedModel()

  val crisisPositions = new Array[(Int, Int)](6)
  var crisisHoverListeners: List[(Crisis, Int) => Unit] = List()

  var gameUI: GameUI = null

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
    var oldHoverSpaceLevel: SpaceLevel = null
    var oldHoverRegion: Region = null
    var oldHoverCrisis: Crisis = null
    var oldHoverCrisisEffect: Int = 4
    override def mousePressed(e: MouseEvent): Unit = {
      if (e.getButton != MouseEvent.BUTTON1) {
        dragging = true
      }
      oldx = e.getX - outerView.getHorizontalScrollBar.getValue
      oldy = e.getY - outerView.getVerticalScrollBar.getValue
    }
    override def mouseReleased(e: MouseEvent): Unit = {
      dragging = false
      if (e.getButton == MouseEvent.BUTTON1) {
        val country = findCountry(e.getX, e.getY)
        if (country != oldHoverCountry) {
          oldHoverCountry = country
        }
        if (country != null) {
          countryClickListeners.foreach(_(country, e.getButton))
        }
      }
    }
    override def mouseMoved(e: MouseEvent): Unit = {
      game match {
        case game: GameTurnZero =>
          if (game.turn == 0 && game.round <= 6) {
            val (crisis, effect) = findCrisis(game, e.getX, e.getY)
            if (crisis != oldHoverCrisis || effect != oldHoverCrisisEffect) {
              oldHoverCrisis = crisis
              oldHoverCrisisEffect = effect
              if (crisis != null) {
                crisisHoverListeners.foreach(_(crisis, effect))
              }
            }
            if (crisis != null) {
              return
            }
          }
        case _ =>
      }
      val country = findCountry(e.getX, e.getY)
      if (country != oldHoverCountry) {
        oldHoverCountry = country
        if (country != null) {
          countryHoverListeners.foreach(_(country))
        }
      }
      val spaceLevel = findSpaceLevel(e.getX, e.getY)
      if (spaceLevel != oldHoverSpaceLevel) {
        oldHoverSpaceLevel = spaceLevel
        if (spaceLevel != null) {
          spaceHoverListeners.foreach(_(spaceLevel))
        }
      }
      val region = findRegion(e.getX, e.getY)
      if (region != oldHoverRegion) {
        oldHoverRegion = region
        if (region != null) {
          regionHoverListeners.foreach(_(region))
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
      game.theWorldMap.countries.values.find(c => {
        val rect = MapValue.getCountryPosSize(c)
        val nx = x / scale
        val ny = y / scale
        val cx = rect._1
        val cy = rect._2
        nx >= cx && nx < cx + rect._3 && ny >= cy && ny < cy + rect._4
      }).orNull
    def findSpaceLevel(x: Int, y: Int): SpaceLevel =
      (0 to 8).map(SpaceLevel.apply).find(l => {
        val (tx, ty) = getTokenCenter(MapValue.space0, MapValue.space8, MapValue.spaceSize, 0, 8, l.level)
        val nx = x / scale
        val ny = y / scale
        val cx = tx - MapValue.spaceSize._1 / 2
        val cy = ty - MapValue.spaceSize._2 / 2
        nx >= cx && nx < cx + MapValue.spaceSize._1 && ny >= cy && ny < cy + MapValue.spaceSize._2
      }).orNull
    def findRegion(x: Int, y: Int): Region =
      (Region.MainRegionSet + Region.SouthEastAsia).find(r => {
        val pos = MapValue.regionPos(r)
        val size = MapValue.regionSize
        val nx = x / scale
        val ny = y / scale
        val cx = pos._1
        val cy = pos._2
        nx >= cx && nx < cx + size._1 && ny >= cy && ny < cy + size._2
      }).orNull
    def findCrisis(game: GameTurnZero, x: Int, y: Int): (Crisis, Int) =
      crisisPositions.lastIndexWhere(pos => {
        if (pos == null) {
          false
        } else {
          val size = (crisisWidth, crisisHeight)
          val nx = x / scale
          val ny = y / scale
          val cx = pos._1 - crisisWidth / 2
          val cy = pos._2 - crisisHeight / 2
          nx >= cx && nx < cx + size._1 && ny >= cy && ny < cy + size._2
        }
      }) match {
        case -1 => (null, 4)
        case n =>
          if (n + 1 > game.round)
            (CrisisUnknown, 4)
          else {
            val crisis = game.crisisDeck(n)
            val ny = y / scale
            val cy = crisisPositions(n)._2 - crisisHeight / 2
            val ty = (ny - cy) * crisisBack.getHeight / crisisHeight
            val effect = (4 to 1 by -1).find(i => {
              val info = CrisisInfo.info((crisis.id + 1) + "-" + i)
              val start = info("start")
              start != null && start.toInt <= ty
            }).getOrElse(5) - 1
            (crisis, effect)
          }
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

    if (game.theWorldMap.countries("Taiwan").isBattlefield) {
      g.drawImage(battlefieldTaiwan, 2209, 712, null)
    }

    if (game.theWorldMap.countries("Zaire").stability == 3) {
      g.drawImage(stability3Zaire, 1209, 1010, null)
    }

    if (game.flags.hasFlag(TZFlags.usGoesFirst)) {
      drawTurnTokenUSFirst(g)
    } else {
      drawTurnToken(g)
    }

    drawDefconToken(g, changedModel.defcon)
    drawVPToken(g, changedModel.vp)

    import me.herbix.ts.logic.Faction._

    val round = if (game.round < 1) 1 else game.round
    val isUS = game.phasingPlayer == US && game.round >= 1
    val roundColor = if (isUS) usColor else ussrColor
    val roundImg = if (isUS) tokenActionUs else tokenActionUssr
    drawActionToken(g, roundColor, roundImg, round)

    drawMilitaryToken(g, usColor, game.military(US), -tokenSize/2, tokenMilitaryUs, changedModel.military(US))
    drawMilitaryToken(g, ussrColor, game.military(USSR), tokenSize/2, tokenMilitaryUssr, changedModel.military(USSR))

    drawSpaceToken(g, usColor, game.space(US).level, -tokenSize/2, tokenSpaceUs, changedModel.space(US))
    drawSpaceToken(g, ussrColor, game.space(USSR).level, tokenSize/2, tokenSpaceUssr, changedModel.space(USSR))

    g.setFont(influenceTokenTextFont)
    val fm = g.getFontMetrics

    for ((name, country) <- game.theWorldMap.normalCountries) {
      val influenceChanged = pendingInfluenceChange.contains(country)
      val usInfluenceChanged = influenceChanged && pendingInfluenceFaction == US
      val ussrInfluenceChanged = influenceChanged && pendingInfluenceFaction == USSR
      val factor = if (pendingInfluenceIsAdd) 1 else -1

      val usInfluence = game.influence(country, US) + factor *
        (if (usInfluenceChanged) pendingInfluenceChange(country) else 0)
      val ussrInfluence = game.influence(country, USSR) + factor *
        (if (ussrInfluenceChanged) pendingInfluenceChange(country) else 0)

      val usDrawColor = if (usInfluenceChanged) usColorInfluenceChange else usColor
      val usBgColor = if (usInfluenceChanged) new Color(255, 255, 255, 128) else Color.WHITE
      val ussrDrawColor = if (ussrInfluenceChanged) ussrColorInfluenceChange else ussrColor
      val ussrBgColor = if (ussrInfluenceChanged) new Color(255, 255, 255, 128) else Color.WHITE

      val (x, y, w, h) = MapValue.getCountryPosSize(country)

      if (pendingCountrySelection(country)) {
        g.setColor(selectedCountry)
        g.fillRect(x, y, w, h)
      } else if (changedCountries.contains(country)) {
        g.setColor(changedCountry(changedCountriesFaction))
        g.fillRect(x, y, w, h)
      } else if (availableCountries.contains(country)) {
        g.setColor(availableCountry)
        g.fillRect(x, y, w, h)
      }

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

      if (changedModel.countries(country)) {
        drawHighLight(g, x, y, w, h)
      }
    }

    val country = game.theWorldMap.countryChina
    val influenceChanged = pendingInfluenceChange.contains(country)
    val ussrInfluenceChanged = influenceChanged && pendingInfluenceFaction == USSR
    val factor = if (pendingInfluenceIsAdd) 1 else -1

    val ussrInfluence = game.influence(country, USSR) + factor *
      (if (ussrInfluenceChanged) pendingInfluenceChange(country) else 0)

    val ussrDrawColor = if (ussrInfluenceChanged) ussrColorInfluenceChange else ussrColor
    val ussrBgColor = if (ussrInfluenceChanged) new Color(255, 255, 255, 128) else Color.WHITE

    val (x, y, w, h) = MapValue.getCountryPosSize(country)

    if (pendingCountrySelection(country)) {
      g.setColor(selectedCountry)
      g.fillRect(x, y, w, h)
    } else if (changedCountries.contains(country)) {
      g.setColor(changedCountry(changedCountriesFaction))
      g.fillRect(x, y, w, h)
    } else if (availableCountries.contains(country)) {
      g.setColor(availableCountry)
      g.fillRect(x, y, w, h)
    }

    if (ussrInfluence > 0) {
      if (ussrInfluence >= country.stability) {
        drawInfluenceToken(g, fm, ussrInfluence.toString, ussrDrawColor, ussrBgColor, x + 8, y + 8)
      } else {
        drawInfluenceToken(g, fm, ussrInfluence.toString, ussrBgColor, ussrDrawColor, x + 8, y + 8)
      }
    }

    if (changedModel.countries(country)) {
      drawHighLight(g, x, y, w, h)
    }

    game match {
      case gameTZ: GameTurnZero => if (game.turn == 0) drawCrisis(g, gameTZ)
      case _ =>
    }

    // TODO remove debug code
    g.setFont(debugAIFont)
    for (country <- game.theWorldMap.normalCountries.values ++ List(game.theWorldMap.countryChina)) {
      if (gameUI != null && gameUI.agent != null && gameUI.agent.isInstanceOf[SimpleAgent]) {
        val (x, y, _, _) = MapValue.getCountryPosSize(country)
        val agent = gameUI.agent.asInstanceOf[SimpleAgent]
        val str = agent.countryState.get(country).map(_.toString).getOrElse("")
        g.setColor(Color.YELLOW)
        g.drawString(str, x, y)
      }
    }
    // debug code end
  }

  val debugAIFont = new Font("Arial", 0, 20)

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

  def drawTokenString(g: Graphics, color: Color, x: Int, y: Int, string: String): Unit = {
    g.setColor(color)
    val fm = g.getFontMetrics
    g.drawString(string, x - fm.stringWidth(string) / 2, y + fm.getHeight / 2 - 4)
  }

  def drawTurnToken(g: Graphics): Unit = {
    val (x, y) = getTokenCenter(MapValue.turnArea, 1, 10, Math.min(game.turn, 10))
    drawToken(g, tokenColor, x - tokenSize / 2, y - tokenSize / 2)
    g.setFont(tokenFont)
    drawTokenString(g, Color.BLACK, x, y, "Turn")
  }

  def drawTurnTokenUSFirst(g: Graphics): Unit = {
    val (x, y) = getTokenCenter(MapValue.turnArea, 1, 10, Math.min(game.turn, 10))
    drawToken(g, usColor, x - tokenSize / 2, y - tokenSize / 2)
    g.setFont(tokenFont)
    drawTokenString(g, Color.WHITE, x, y - 8, "Turn")
    g.setFont(tokenFontSmall)
    drawTokenString(g, Color.WHITE, x, y + 12, "US First")
  }

  def drawDefconToken(g: Graphics, highlighted: Boolean): Unit = {
    val (x, y) = getTokenCenter(MapValue.defcon1, MapValue.defcon5, MapValue.defconSize, 1, 5, game.defcon)
    val img = tokenDefcon
    drawToken(g, tokenColor, x - tokenSize / 2, y - tokenSize / 2)
    g.drawImage(img, x - img.getWidth / 2, y - img.getHeight / 2, null)
    if (highlighted) {
      drawHighLight(g, x-tokenSize/2, y-tokenSize/2, tokenSize, tokenSize)
    }
  }

  def drawVPToken(g: Graphics, highlighted: Boolean): Unit = {
    val (x, y) = getTokenCenter(MapValue.vpUs20, MapValue.vpUssr20, MapValue.vpSize, 20, -20,
      Math.max(-20, Math.min(20, game.vp)))
    drawToken(g, tokenColor, x - tokenSize / 2, y - tokenSize / 2)
    g.setFont(tokenFontLarger)
    drawTokenString(g, Color.BLACK, x, y, "VP")
    if (highlighted) {
      drawHighLight(g, x-tokenSize/2, y-tokenSize/2, tokenSize, tokenSize)
    }
  }

  def drawActionToken(g: Graphics, bgColor: Color, img: BufferedImage, value: Int): Unit = {
    val (x, y) = getTokenCenter(MapValue.round1, MapValue.round8, MapValue.roundSize, 1, 8, value)
    drawToken(g, bgColor, x - tokenSize / 2, y - tokenSize / 2)
    g.drawImage(img, x - img.getWidth / 2, y - img.getHeight / 2, null)
  }

  def drawSpaceToken(g: Graphics, bgColor: Color, level: Int, yOffset: Int, img: BufferedImage, highlighted: Boolean): Unit = {
    val (x, y) = getTokenCenter(MapValue.space0, MapValue.space8, MapValue.spaceSize, 0, 8, level)
    drawToken(g, bgColor, x - tokenSize / 2, y - tokenSize / 2 + yOffset)
    g.drawImage(img, x - img.getWidth / 2, y - img.getHeight / 2 + yOffset, null)
    if (highlighted) {
      drawHighLight(g, x-tokenSize/2, y-tokenSize/2+yOffset, tokenSize, tokenSize)
    }
  }

  def drawMilitaryToken(g: Graphics, bgColor: Color, level: Int, yOffset: Int, img: BufferedImage, highlighted: Boolean): Unit = {
    val (x, y) = getTokenCenter(MapValue.military0, MapValue.military5, MapValue.militarySize, 0, 5, level)
    drawToken(g, bgColor, x - tokenSize / 2, y - tokenSize / 2 + yOffset)
    g.drawImage(img, x - img.getWidth / 2, y - img.getHeight / 2 + yOffset, null)
    if (highlighted) {
      drawHighLight(g, x-tokenSize/2, y-tokenSize/2+yOffset, tokenSize, tokenSize)
    }
  }

  def drawHighLight(g: Graphics, x: Int, y: Int, w: Int, h: Int): Unit = {
    val g2d = g.asInstanceOf[Graphics2D]
    val stroke = g2d.getStroke
    g2d.setStroke(new BasicStroke(9, BasicStroke.CAP_SQUARE, BasicStroke.JOIN_MITER, 10.0f, Array(22f, 18f), 1 + changedModel.tick % 40))
    g.setColor(Color.BLACK)
    g.drawRect(x, y, w, h)
    g2d.setStroke(new BasicStroke(7, BasicStroke.CAP_SQUARE, BasicStroke.JOIN_MITER, 10.0f, Array(20f), changedModel.tick % 40))
    g.setColor(Resource.mapHighlightColor)
    g.drawRect(x, y, w, h)
    g2d.setStroke(stroke)
  }

  def drawCrisis(g: Graphics, game: GameTurnZero): Unit = {
    val crisisProgress = game.round - 1
    if (crisisProgress > 6) {
      return
    }
    for (i <- 0 to 5) {
      val position = crisisPositions(i)
      val image =
        if (i <= crisisProgress)
          Resource.crisis(game.crisisDeck(i).id)
        else
          Resource.crisisBack
      drawCrisisCard(g, position._1, position._2, image)
      if (i < crisisProgress) {
        val start = CrisisInfo.info((game.crisisDeck(i).id + 1) + "-" + (game.crisisEffect(i) + 1))("start").toInt
        val g2d = g.asInstanceOf[Graphics2D]
        val stroke = g2d.getStroke
        g2d.setStroke(new BasicStroke(3))
        g2d.setColor(Color.RED)
        g2d.drawOval(position._1 - crisisWidth / 2 + 25, position._2 - crisisHeight / 2 + start * crisisHeight / crisisBack.getHeight + 5, 30, 30)
        g2d.setStroke(stroke)
      }
    }
  }

  def drawCrisisCard(g: Graphics, x: Int, y: Int, image: BufferedImage) = {
    g.drawImage(image, x - crisisWidth / 2, y - crisisHeight / 2, crisisWidth, crisisHeight, null)
    g.setColor(Color.DARK_GRAY)
    g.drawRect(x - crisisWidth / 2, y - crisisHeight / 2, crisisWidth, crisisHeight)
  }

  def updateCrisisPositions(): Unit = {
    game match {
      case game: GameTurnZero =>
        if (game.turn != 0) return
        val crisisProgress = game.round - 1
        var x = 750
        for (i <- 0 to crisisProgress) {
          crisisPositions(i) = (x, 1100)
          x += 100
        }
        x = 1750
        for (i <- crisisProgress + 1 to 5) {
          crisisPositions(i) = (x, 1100)
          x -= 100
        }
      case _ =>
    }
  }

  def updateChangedModel(): Unit = {
    val target = {
      val reverseHistory = game.historyDesc.toStream.reverse
      val index = {
        val f = if (game.operatingPlayer == Faction.Neutral) {
          game.playerFaction
        } else {
          game.operatingPlayer
        }

        var faction: Faction = null
        var oldIndex = 1
        var index = 0
        do {
          index = reverseHistory.indexWhere(_.canOperate, oldIndex)

          if (index != -1) {
            if (faction == null) {
              faction = reverseHistory.drop(index).head.operatingPlayer
            } else {
              if (faction == Faction.Neutral) {
                index = -1
              } else if (faction != reverseHistory.drop(index).head.operatingPlayer) {
                index = -1
              }
            }
          }

          if (index >= 1) {
            oldIndex = index + 1
          }
        } while (index >= 0)

        oldIndex - 1
      }

      if (index == -1) {
        game.historyDesc
      } else {
        game.historyDesc.takeRight(index)
      }
    }

    changedModel.reset()

    for (h <- target) {
      h match {
        case h: HistoryCountry =>
          changedModel.countries ++= h.countries
        case h: HistoryDefcon =>
          changedModel.defcon = true
        case h: HistoryVp =>
          changedModel.vp = true
        case h: HistoryMilitary =>
          changedModel.military += h.faction
        case h: HistorySpace =>
          changedModel.space += h.faction
        case _ =>
      }
    }
  }

  class ChangedModel {
    var countries = Set.empty[Country]
    var defcon = false
    var vp = false
    var military = Set.empty[Faction]
    var space = Set.empty[Faction]
    var tick = 0

    val timer = new Timer(60, new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        tick += 1
        repaint()
      }
    })
    timer.start()

    def reset(): Unit = {
      countries = Set.empty[Country]
      defcon = false
      military = Set.empty[Faction]
      space = Set.empty[Faction]
      vp = false
    }
  }

}
