package me.herbix.ts.ui

import java.awt.event._
import java.awt.{RenderingHints, Graphics2D, Graphics, Dimension}
import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import javax.swing.{SwingUtilities, JViewport, JScrollPane, JPanel}

import me.herbix.ts.logic.Game
import me.herbix.ts.util.Resource

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

  val bg = Resource.worldMap

  setPreferredSize(new Dimension((bg.getWidth * scale).toInt, (bg.getHeight * scale).toInt))

  override def paint(g: Graphics): Unit = {
    super.paint(g)

    val g2d = g.asInstanceOf[Graphics2D]

    g2d.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BILINEAR)
    g2d.scale(scale, scale)
    g.drawImage(bg, 0, 0, null)
  }

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
}
