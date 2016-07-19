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

  val host = properties.getOrDefault("host", "localhost").asInstanceOf[String]
  val port = properties.getOrDefault("port", "23981").asInstanceOf[String].toInt
  val pidFile = properties.getOrDefault("pid-file", null).asInstanceOf[String]

}
