package me.herbix.ts.logic

/**
  * Created by Chaofan on 2016/6/13.
  */
object State extends Enumeration {
  type State = Value

  val start = Value
  val waitOther = Value
  val putStartUSSR, putStartUS, putStartUSExtra = Value
  val selectHeadlineCard, solveHeadLineCard1, solveHeadLineCard2 = Value
  val selectCardAndAction = Value
  val cardE, cardO, cardOE, cardEO = Value
  val cardEvent, cardOperation = Value
  val cardOperationSelect, cardOperationAddInfluence, cardOperationRealignment, cardOperationCoup = Value
  val cardEventStart, cardEvent1, cardEvent2, cardEvent3, cardEvent4, cardEvent5, cardEventEnd = Value

}
