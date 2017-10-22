// Copyright (C) 2017 Chaofan

package me.herbix.ts.util

import java.io.InputStream

/**
  * Created by Chaofan on 2017/10/22.
  */
object Localization {
  private val RESOURCE_FOLDER_PRIMARY = System.getProperty("user.language", "en") + "-" + System.getProperty("user.country", "US")
  private val RESOURCE_FOLDER_SECONDARY = System.getProperty("user.language", "en")

  def getResourceAsStream(path: String): InputStream = {
    if (!path.startsWith("/")) {
      return getResourceAsStreamImpl(path)
    }

    val primary = getResourceAsStreamImpl("/" + RESOURCE_FOLDER_PRIMARY + path)
    if (primary != null) {
      return primary
    }

    val secondary = getResourceAsStreamImpl("/" + RESOURCE_FOLDER_SECONDARY + path)
    if (secondary != null) {
      return secondary
    }

    getResourceAsStreamImpl(path)
  }

  private def getResourceAsStreamImpl(path: String): InputStream = {
    getClass.getResourceAsStream(path)
  }

  def string(id: String): String = {
    StringInfo.info.get(id) match {
      case Some(item) => item.lines.map(_.content).mkString("\n")
      case None => id
    }
  }
}
