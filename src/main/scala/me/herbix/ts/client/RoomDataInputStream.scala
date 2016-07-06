package me.herbix.ts.client

import java.io.{IOException, OutputStream, InputStream}

import com.sun.scenario.effect.Offset

import scala.collection.mutable

/**
  * Created by Chaofan on 2016/7/5.
  */
class RoomDataInputStream extends InputStream {

  var offset = 0
  var current: Array[Byte] = null
  val queue = new mutable.Queue[Array[Byte]]

  var closed = false

  override def read(): Int = {
    if (closed) {
      return -1
    }
    if (current == null) {
      this.synchronized {
        this.wait()
      }
    }
    if (closed) {
      return -1
    }
    val r = current(offset) & 0xFF

    offset += 1
    if (offset >= current.length) {
      if (queue.nonEmpty) {
        current = queue.dequeue()
      } else {
        current = null
      }
      offset = 0
    }

    r
  }

  override def read(arr: Array[Byte], offset: Int, length: Int): Int = {
    val b1 = read()
    if (b1 < 0) {
      return -1
    }

    arr(offset) = b1.toByte

    var off = offset + 1
    var len = length - 1

    while (len > 0 && current != null) {
      val copied = Math.min(len, current.length - this.offset)
      System.arraycopy(current, this.offset, arr, off, copied)
      off += copied
      len -= copied
      this.offset += copied
      if (this.offset >= current.length) {
        if (queue.nonEmpty) {
          current = queue.dequeue()
        } else {
          current = null
        }
        this.offset = 0
      }
    }

    println(f"length=$length len=$len")
    length - len
  }

  def fill(buffer: Array[Byte]): Unit = {
    if (current == null) {
      current = buffer
    } else {
      queue.enqueue(buffer)
    }
    this.synchronized {
      this.notify()
    }
  }

  override def close(): Unit = {
    closed = true
    this.synchronized {
      this.notify()
    }
  }
}
