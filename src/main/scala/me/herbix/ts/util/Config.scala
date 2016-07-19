package me.herbix.ts.util

import java.util.Properties

/**
  * Created by Chaofan on 2016/7/18.
  */
object Config {

  private val properties = new Properties

  try {
    properties.load(getClass.getResourceAsStream("/config.property"))
  } catch {
    case e: Throwable => e.printStackTrace()
  }

  val host = properties.getProperty("host", "localhost")
  val port = properties.getProperty("port", "23981").toInt
  val pidFile = properties.getProperty("pid-file", null)

}
