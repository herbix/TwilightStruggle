package me.herbix.ts.logic

import java.io.{ByteArrayInputStream, DataInputStream, DataOutputStream, ByteArrayOutputStream}

import me.herbix.ts.util.Serializer

/**
  * Created by Chaofan on 2016/7/14.
  */
class Snapshot(val game: Game) {

  private val data = createSnapshotData(game)

  val operatingPlayer = game.operatingPlayer
  var needApproval = false

  def createSnapshotData(game: Game): Array[Byte] = {
    val out = new ByteArrayOutputStream
    Serializer.writeGameState(game, new DataOutputStream(out))
    out.toByteArray
  }

  def rollBack(): Unit = {
    Serializer.readGameState(game, new DataInputStream(new ByteArrayInputStream(data)))
    game.lastSnapshot = this
  }
}
