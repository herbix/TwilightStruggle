// Copyright (C) 2017 Chaofan

package me.herbix.ts.ui

import java.awt.{Color, Graphics}
import javax.swing.{ButtonModel, JButton}

/**
  * Created by Chaofan on 2016/9/19.
  */
class Button extends JButton {

  import Button._

  override def paintComponent(g: Graphics): Unit = {
    val w = getWidth
    val h = getHeight
    val m = model

    if (isOpaque) {
      g.setColor(normalBgColor)
      g.fillRect(0, 0, w, h)
    }

    if (!m.isEnabled) {
      g.setColor(disabledBgColor)
      g.fillRect(1, 1, w-2, h-2)
    } else if (m.isPressed) {
      g.setColor(pressedBgColor)
      g.fillRect(1, 1, w-2, h-2)
    } else if (m.isRollover) {
      g.setColor(hoverBgColor)
      g.fillRect(1, 1, w-2, h-2)
    }

    paintIcon(g, w, h, m)

    g.setColor(borderColor)
    g.drawRect(1, 1, w-2, h-2)

    paintText(g, w, h, m)
  }

  def paintIcon(g: Graphics, w: Int, h: Int, m: ButtonModel): Unit = {
    val icon = if (m.isEnabled) getIcon else getDisabledIcon
    if (icon == null) {
      return
    }

    icon.paintIcon(this, g, 3, (h - icon.getIconHeight + 1) / 2)
  }

  def paintText(g: Graphics, w: Int, h: Int, m: ButtonModel): Unit = {
    val fm = g.getFontMetrics
    val str = getText

    val y = (h - fm.getHeight) / 2 + fm.getAscent + (if (m.isPressed && m.isEnabled) 1 else 0)
    val x = if (getIcon == null)
      (w - fm.stringWidth(str)) / 2
    else if (getIcon.getIconWidth + 3 > (w - fm.stringWidth(str)) / 2)
      getIcon.getIconWidth + 3 + (w - getIcon.getIconWidth - 3 - fm.stringWidth(str)) / 2
    else
      (w - fm.stringWidth(str)) / 2

    if (m.isEnabled) {
      g.setColor(normalTextColor)
    } else {
      g.setColor(disabledTextColor)
    }

    g.drawString(str, x, y)
  }
}

object Button {
  val borderColor = new Color(0x888888)
  val normalBgColor = new Color(0xEEEEEE)
  val pressedBgColor = new Color(0xCCCCCC)
  val hoverBgColor = new Color(0xF8F8F8)
  val disabledBgColor = new Color(0xAAAAAA)

  val normalTextColor = Color.BLACK
  val disabledTextColor = new Color(0x888888)
}