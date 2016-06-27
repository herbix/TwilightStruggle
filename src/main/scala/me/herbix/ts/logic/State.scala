package me.herbix.ts.logic

/**
  * Created by Chaofan on 2016/6/13.
  */
object State extends Enumeration {
  type State = Value

  val start = Value
  val waitOther = Value
  val putStartUSSR, putStartUS, putStartUSExtra = Value
  val selectHeadlineCard, selectHeadlineCard2, solveHeadLineCard1, solveHeadLineCard2 = Value
  val selectCardAndAction = Value
  val cardE, cardO, cardOE, cardEO = Value
  val cardEvent, cardOperation = Value
  val cardOperationSelect, cardOperationAddInfluence, cardOperationRealignment, cardOperationCoup = Value
  val discardHeldCard = Value
  val selectTake8Rounds = Value
  val quagmireDiscard, quagmirePlayScoringCard = Value

  object cardEventStep {
    val cardEventStepImpl = (0 to 10).map(i => Value).toArray
    val cardEventStepInverse = cardEventStepImpl.indices.map(i => (cardEventStepImpl(i), i)).toMap
    def apply(i: Int): State = cardEventStepImpl(i)
    def unapply(state: State): Option[Int] = cardEventStepInverse.get(state)
  }
  val cardEventStart = cardEventStep(0)
  val cardEventEnd = Value

  val cardEventInfluence, cardEventSelectCardOrCancel, cardEventYesNo, cardEventSelectCountry = Value

  object EventStates {
    val cardEventStates = Set(cardEventInfluence, cardEventSelectCardOrCancel, cardEventYesNo)
    def unapply(state: State): Option[Boolean] = {
      if (cardEventStates(state)) Some(true) else None
    }
  }

}
