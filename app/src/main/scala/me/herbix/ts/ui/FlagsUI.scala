package me.herbix.ts.ui

import java.awt.event.{MouseAdapter, MouseListener, MouseEvent, MouseMotionAdapter}
import java.awt.{Color, RenderingHints, Graphics2D, Graphics}
import javax.swing.JPanel

import me.herbix.ts.logic.Faction.Faction
import me.herbix.ts.logic.{Flag, Game}
import me.herbix.ts.util.Resource

/**
  * Created by Chaofan on 2016/6/17.
  */
class FlagsUI(game: Game) extends JPanel {
  game.stateUpdateListeners :+= (() => repaint())

  val flags = new Array[(Faction, Flag)](110)
  var hoverFlag: (Faction, Flag) = null

  var flagClickListeners: List[(Faction, Flag, Any) => Unit] = List()
  var flagHoverListeners: List[(Faction, Flag, Any) => Unit] = List()

  override def paint(graphics: Graphics): Unit = {
    super.paint(graphics)

    val g = graphics.asInstanceOf[Graphics2D]

    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    g.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BILINEAR)

    var x = 4
    var y = 10
    var n = 0

    g.setColor(Color.BLACK)
    for ((faction, factionFlags) <- game.flags.flagSets) {
      for (flag <- factionFlags) {
        g.drawImage(Resource.flag(flag.id)(faction), x, y, null)
        g.drawRect(x, y, 45, 45)
        flags(n) = (faction, flag)
        n += 1
        x += 49
        if (x >= 200) {
          x = 4
          y += 49
        }
      }
    }

    for (i <- n until flags.length) {
      flags(i) = null
    }
  }

  addMouseMotionListener(new MouseMotionAdapter {
    override def mouseMoved(e: MouseEvent): Unit = {
      val x = e.getX - 4
      val y = e.getY - 10
      val hover = (x % 49) < 45 && (y % 49) < 45
      if (hover) {
        val id = (x / 49) + (y / 49) * 4
        if (id < flags.length) {
          val flag = flags(id)
          if (flag != hoverFlag) {
            hoverFlag = flag
            if (flag != null) {
              flagHoverListeners.foreach(_(flag._1, flag._2, game.flags.getFlagData(flag._1, flag._2)))
            }
          }
        }
      }
    }
  })

  addMouseListener(new MouseAdapter {
    override def mouseClicked(e: MouseEvent): Unit = {
      if (hoverFlag != null) {
        val flag = hoverFlag
        flagClickListeners.foreach(_(flag._1, flag._2, game.flags.getFlagData(flag._1, flag._2)))
      }
    }
  })
}
