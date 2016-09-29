package me.herbix.ts.util

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, DataInputStream, DataOutputStream}

import me.herbix.ts.logic.GameRecordingHistory

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
