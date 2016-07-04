package me.herbix.ts.server

import java.net.ServerSocket

import scala.collection.mutable

/**
  * Created by Chaofan on 2016/7/3.
  */
object Server {
  val netHandlers = mutable.Map.empty[Int, NetHandlerServer]
  val rooms = mutable.Map.empty[Int, Room]

  def main(args: Array[String]): Unit = {
    val serverSocket = new ServerSocket(23981)
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
}
