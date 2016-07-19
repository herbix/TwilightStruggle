package me.herbix.ts.server

import java.io.FileWriter
import java.lang.management.ManagementFactory
import java.net.ServerSocket

import me.herbix.ts.util.Config

import scala.collection.mutable

/**
  * Created by Chaofan on 2016/7/3.
  */
object Server {
  val netHandlers = mutable.Map.empty[Int, NetHandlerServer]
  val rooms = mutable.Map.empty[Int, Room]

  def main(args: Array[String]): Unit = {
    if (Config.pidFile != null) {
      savePidFile()
    }

    val serverSocket = new ServerSocket(Config.port)
    while (true) {
      val socket = serverSocket.accept()
      println(s"Receive connection $socket")
      new NetHandlerServer(socket)
    }
  }

  var id = 0
  def nextId(): Int = {
    id += 1
    id
  }

  private def savePidFile() {
    try {
      val fw = new FileWriter(Config.pidFile)
      fw.write(getPid)
      fw.close()
    } catch {
      case e: Exception => println("Cannot create pid file '" + Config.pidFile + "'")
    }
  }

  private def getPid: String = {
    ManagementFactory.getRuntimeMXBean.getName.split("@")(0)
  }
}
