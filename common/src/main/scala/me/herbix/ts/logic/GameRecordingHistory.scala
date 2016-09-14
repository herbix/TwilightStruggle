package me.herbix.ts.logic

/**
  * Created by Chaofan on 2016/9/14.
  */
class GameRecordingHistory extends Game {

  // history
  var currentHistoryId = 0
  var currentHistory = List.empty[History]
  var oldHistory = List.empty[History]
  def history = currentHistory ++ oldHistory

  // snapshot
  private var lastSnapshot = new Snapshot(this)

  override protected def nextStateContainsException(input: Operation): Unit = {
    super.nextStateContainsException(input)

    if (currentHistory.nonEmpty) {
      val newestHistory = currentHistory.last
      if (newestHistory.snapshot == null) {
        newestHistory.snapshot = lastSnapshot
      }
    }
    lastSnapshot = new Snapshot(this)
  }

  override def recordHistory(h: History): Unit = {
    super.recordHistory(h)

    h.id = currentHistoryId
    currentHistoryId += 1
    if (h.isInstanceOf[HistoryTurnRound]) {
      oldHistory = currentHistory ++ oldHistory
      currentHistory = List.empty
      clearSnapshots()
    }
    currentHistory :+= h
  }

  override def clearSnapshots(): Unit = {
    lastSnapshot.needApproval = true
    for (h <- history) {
      if (h.snapshot != null) {
        h.snapshot.needApproval = true
      }
    }
  }

  override def rollBackBeforeHistory(historyId: Int): Unit = {
    history.find(_.id == historyId) match {
      case Some(history) =>
        val snapshot = history.snapshot
        if (snapshot != null && !snapshot.needApproval) {
          snapshot.rollBack()
          lastSnapshot = snapshot
        }
      case _ =>
    }
    currentOperationHint = createOperationHint()
    stateUpdateListeners.foreach(_())
  }

}
