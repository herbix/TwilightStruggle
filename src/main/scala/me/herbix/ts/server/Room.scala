package me.herbix.ts.server

import scala.collection.mutable

/**
  * Created by Chaofan on 2016/7/3.
  */
class Room {
  val id: Int = Server.nextId()
  val netHandlers = mutable.Set.empty[NetHandlerServer]

  Server.rooms += id -> this
  Server.netHandlers.values.foreach(_.sendNewRoom(this))

  def leave(netHandler: NetHandlerServer): Unit = {
    netHandlers -= netHandler
    if (netHandlers.isEmpty) {
      Server.rooms -= id
      Server.netHandlers.values.foreach(_.sendDestroyRoom(this))
    } else {
      for (nh <- netHandlers) {
        nh.sendLeaveRoom(netHandler)
      }
    }
  }

  def join(netHandler: NetHandlerServer): Unit = {
    netHandlers += netHandler
    for (nh <- netHandlers) {
      if (nh != netHandler) {
        nh.sendOtherJoinRoom(netHandler)
      }
    }
  }

  def roomDataExcept(buffer: Array[Byte], netHandler: NetHandlerServer): Unit = {
    for (nh <- netHandlers) {
      if (nh != netHandler) {
        nh.sendData(buffer)
      }
    }
  }

}
