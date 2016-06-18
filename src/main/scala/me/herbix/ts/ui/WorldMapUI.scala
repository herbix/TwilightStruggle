package me.herbix.ts.ui

import java.awt.event._
import java.awt._
import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import javax.swing.{SwingUtilities, JViewport, JScrollPane, JPanel}

import me.herbix.ts.logic.{Faction, Game}
import me.herbix.ts.util.{MapValue, Resource}
import Resource._

/**
  * Created by Chaofan on 2016/6/14.
  */
class WorldMapUI(val game: Game) extends JPanel {

  var outerView: JScrollPane = null

  def setOuter(worldMapUIOuter: JScrollPane) = {
    outerView = worldMapUIOuter
    outerView.addComponentListener(new ComponentAdapter {
      override def componentResized(e: ComponentEvent): Unit = {
        minscale = Math.max((outerView.getWidth - 2).toDouble / bg.getWidth, (outerView.getHeight - 2).toDouble / bg.getHeight)
        adjustScale()
      }
    })
  }

  var minscale = 0.299
  var scale = 0.7

  val bg = worldMap

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
    override def mouseDragged(e: MouseEvent): Unit = {
      val x = e.getX - outerView.getHorizontalScrollBar.getValue
      val y = e.getY - outerView.getVerticalScrollBar.getValue
      outerView.getHorizontalScrollBar.setValue(outerView.getHorizontalScrollBar.getValue - x + oldx)
      outerView.getVerticalScrollBar.setValue(outerView.getVerticalScrollBar.getValue - y + oldy)
      oldx = x
      oldy = y
    }
  }

  addMouseListener(mouseAdapter)
  addMouseMotionListener(mouseAdapter)

  override def paint(g: Graphics): Unit = {
    super.paint(g)

    val g2d = g.asInstanceOf[Graphics2D]

    g2d.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BILINEAR)
    g2d.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON)
    g2d.scale(scale, scale)
    g.drawImage(bg, 0, 0, null)

    drawToken(g, tokenColor,
      (MapValue.turnArea._1 * (21 - game.turn * 2) + MapValue.turnArea._3 * (game.turn * 2 - 1) - tokenSize * 10) / 20,
      (MapValue.turnArea._2 + MapValue.turnArea._4 - tokenSize) / 2
    )

    drawToken(g, tokenColor,
      (MapValue.defcon5._1 * (game.defcon - 1) + MapValue.defcon1._1 * (5 - game.defcon)) / 4 + (MapValue.defconSize._1 - tokenSize) / 2,
      MapValue.defcon5._2 + (MapValue.defconSize._2 - tokenSize) / 2
    )

    drawToken(g, tokenColor,
      (MapValue.vpUssr20._1 * (20 - game.vp) + MapValue.vpUs20._1 * (game.vp + 20)) / 40 + (MapValue.vpSize._1 - tokenSize) / 2,
      MapValue.vpUssr20._2 + (MapValue.vpSize._2 - tokenSize) / 2
    )

    import me.herbix.ts.logic.Faction._

    val round = if (game.round < 1) 1 else game.round
    drawToken(g, if (game.current == US) usColor else ussrColor,
      (MapValue.round1._1 * (8 - round) + MapValue.round8._1 * (round - 1)) / 7 + (MapValue.roundSize._1 - tokenSize) / 2,
      MapValue.round1._2 + (MapValue.roundSize._2 - tokenSize) / 2
    )

    drawToken(g, usColor,
      (MapValue.military5._1 * game.military(US) + MapValue.military0._1 * (5 - game.military(US))) / 5 + (MapValue.militarySize._1 - tokenSize) / 2,
      MapValue.military5._2 + (MapValue.militarySize._2 - tokenSize * 2) / 2
    )

    drawToken(g, ussrColor,
      (MapValue.military5._1 * game.military(USSR) + MapValue.military0._1 * (5 - game.military(USSR))) / 5 + (MapValue.militarySize._1 - tokenSize) / 2,
      MapValue.military5._2 + MapValue.militarySize._2 / 2
    )

    drawToken(g, usColor,
      (MapValue.space8._1 * game.space(US) + MapValue.space0._1 * (8 - game.space(US))) / 8 + (MapValue.spaceSize._1 - tokenSize) / 2,
      MapValue.space8._2 + (MapValue.spaceSize._2 - tokenSize * 2) / 2
    )

    drawToken(g, ussrColor,
      (MapValue.space8._1 * game.space(USSR) + MapValue.space0._1 * (8 - game.space(USSR))) / 8 + (MapValue.spaceSize._1 - tokenSize) / 2,
      MapValue.space8._2 + MapValue.spaceSize._2 / 2
    )

    g.setFont(influenceTokenTextFont)
    val fm = g.getFontMetrics

    for ((name, country) <- game.worldMap.normalCountries) {
      val usInfluence = country.influence(US)
      val ussrInfluence = country.influence(USSR)
      val (x, y) = MapValue.countryPosMap(name)
      if (usInfluence > 0) {
        if (usInfluence - ussrInfluence >= country.stability) {
          drawInfluenceToken(g, fm, usInfluence.toString, usColor, Color.WHITE, x + 2, y + 18)
        } else {
          drawInfluenceToken(g, fm, usInfluence.toString, Color.WHITE, usColor, x + 2, y + 18)
        }
      }
      if (ussrInfluence > 0) {
        if (ussrInfluence - usInfluence >= country.stability) {
          drawInfluenceToken(g, fm, ussrInfluence.toString, ussrColor, Color.WHITE, x + 52, y + 18)
        } else {
          drawInfluenceToken(g, fm, ussrInfluence.toString, Color.WHITE, ussrColor, x + 52, y + 18)
        }
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
}
