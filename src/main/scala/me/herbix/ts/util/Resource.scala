package me.herbix.ts.util

import java.awt.{Color, Font}
import javax.imageio.ImageIO

/**
  * Created by Chaofan on 2016/6/17.
  */
object Resource {
  val worldMap = ImageIO.read(getClass.getResourceAsStream("/worldmap.jpg"))
  val card = (0 to 110)
    .map(i => (i, ImageIO.read(getClass.getResourceAsStream(f"/cards/$i%03d.png"))))
    .toMap

  val textFont = new Font(Lang.heiti, 0, 16)
  val textColor = Color.DARK_GRAY
  val usColor = Color.BLUE
  val ussrColor = Color.RED

  val influenceTokenTextFont = new Font(Lang.heiti, Font.BOLD, 32)
  val tokenColor = Color.YELLOW
  val tokenSize = 46
}
