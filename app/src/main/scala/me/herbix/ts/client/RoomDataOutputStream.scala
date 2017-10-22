// Copyright (C) 2017 Chaofan

package me.herbix.ts.client

import java.io.{ByteArrayOutputStream, OutputStream}

/**
  * Created by Chaofan on 2016/7/5.
  */
class RoomDataOutputStream(netHandler: NetHandlerClient) extends OutputStream {

  val out = new ByteArrayOutputStream

  override def write(b: Int): Unit = {
    out.write(b)
  }

  override def write(array: Array[Byte], offset: Int, length: Int): Unit = {
    out.write(array, offset, length)
  }

  override def flush(): Unit = {
    netHandler.sendRoomData(out.toByteArray)
    out.reset()
  }

}
