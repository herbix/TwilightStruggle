package me.herbix.ts.client

import me.herbix.ts.logic.{Operation, GameTrait}

/**
  * Created by Chaofan on 2016/7/5.
  */
class RemoteGame(netHandler: NetHandlerClient) extends GameTrait {
  override def nextState(input: Operation): Unit = {
    netHandler.roomSendOperation(input)
  }
}
