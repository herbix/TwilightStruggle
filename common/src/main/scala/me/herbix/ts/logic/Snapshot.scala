package me.herbix.ts.logic

import java.io.{ByteArrayInputStream, DataInputStream, DataOutputStream, ByteArrayOutputStream}

import me.herbix.ts.util.Serializer

/**
  * Created by Chaofan on 2016/7/14.
  */
class Snapshot(val game: GameRecordingHistory) {

  private val data = createSnapshotData(game)

  val operatingPlayer = game.operatingPlayer
  var needApproval = false

  def createSnapshotData(game: GameRecordingHistory): Array[Byte] = {
    val out = new ByteArrayOutputStream
    Serializer.writeGameRecordingHistoryState(game, new DataOutputStream(out))
    out.toByteArray
  }

  def rollBack(): Unit = {
    Serializer.readGameRecordingHistoryState(game, new DataInputStream(new ByteArrayInputStream(data)))
  }
}
