package me.herbix.ts.util

import java.io.{InputStreamReader, BufferedReader, InputStream}

import scala.collection.mutable

/**
  * Created by Chaofan on 2016/9/6.
  */
class Info(in: InputStream) {

  val infoItems: Seq[InfoItem] = loadInfoFrom(in)

  private def loadInfoFrom(in: InputStream): Seq[InfoItem] = {
    val reader = new BufferedReader(new InputStreamReader(in, "UTF-8"))

    val properties = mutable.Map.empty[String, String]
    var lines = mutable.Buffer.empty[InfoLine]
    var name = ""
    var first = true

    val result = mutable.Buffer.empty[InfoItem]

    while (true) {
      val line = reader.readLine()
      if (line == null) {
        if (!first) {
          result += new InfoItem(name, properties.toMap, lines)
        }
        return result.toSeq
      }

      if (line.length == 0) {

      } else if (line.startsWith("[")) {
        if (!first) {
          result += new InfoItem(name, properties.toMap, lines)
          properties.clear()
          lines = mutable.Buffer.empty[InfoLine]
          name = ""
        }
        first = false
        val str = line.substring(1, line.length - 1)
        val split = str.split("[\\s]+")
        name = split(0)
        properties ++= split.toStream.takeRight(split.length - 1).map(_.trim).map(s => {
          s.indexOf('=') match {
            case -1 => (s, "true")
            case index => (s.substring(0, index), s.substring(index + 1))
          }
        }).toMap
      } else if (line.startsWith("#")) {
          val lastIndex = line.lastIndexOf('#')
          val propertyString = line.substring(1, lastIndex)
          val str = line.substring(lastIndex + 1)
          val split = propertyString.split("#")
          lines += new InfoLine(str,
            split.toStream.filter(_.length > 0).map(_.trim).map(s => {
              s.indexOf('=') match {
                case -1 => (s, "")
                case index => (s.substring(0, index), s.substring(index + 1))
              }
            }).toMap
          )
      } else {
        lines += new InfoLine(line)
      }
    }

    result.toSeq
  }
}

case class InfoItem(val name: String, val properties: Map[String, String], val lines: Seq[InfoLine]) {
  def apply(key: String): String = {
    properties.get(key).orNull
  }
  def apply(key: Int): InfoLine = {
    lines(key)
  }
}

case class InfoLine(val content: String, val properties: Map[String, String] = Map.empty) {
  def apply(key: String): String = {
    properties.get(key).orNull
  }
}

object CardInfo extends Info(getClass.getResourceAsStream("/cards.txt")) {
  val info = infoItems.map(item => (item.name.toInt, item)).toMap
}
