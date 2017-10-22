// Copyright (C) 2017 Chaofan

package me.herbix.ts.logic

/**
  * Created by Chaofan on 2016/6/13.
  */
object State extends Enumeration {
  type State = Value
  def newState = Value

  val start = Value
  val waitOther = Value
  val putStartUSSR, putStartUS, putStartExtra = Value
  val selectHeadlineCard, selectHeadlineCard2, solveHeadLineCard1, solveHeadLineCard2 = Value
  val selectCardAndAction, selectAction = Value
  val cardE, cardO, cardOE, cardEO = Value
  val cardEvent, cardOperation = Value
  val cardOperationSelect, cardOperationAddInfluence, cardOperationRealignment, cardOperationCoup = Value
  val discardHeldCard = Value
  val selectTake8Rounds = Value
  val quagmireDiscard, quagmirePlayScoringCard = Value
  val noradInfluence = Value
  val cubaMissileRemove = Value
  val kremlinFluPlayScoringCard = Value
  val end = Value

  object cardEventStep {
    val cardEventStepImpl = (0 to 10).map(i => Value).toArray
    val cardEventStepInverse = cardEventStepImpl.indices.map(i => (cardEventStepImpl(i), i)).toMap
    def apply(i: Int): State = cardEventStepImpl(i)
    def unapply(state: State): Option[Int] = cardEventStepInverse.get(state)
  }
  val cardEventStart = cardEventStep(0)
  val cardEventEnd = Value

  val cardEventInfluence, cardEventSelectCardOrCancel, cardEventYesNo, cardEventSelectCountry = Value
  val cardEventOperation, cardEventConfirm, cardEventSelectCard, cardEventAnotherCard = Value
  val cardEventSelectMultipleCards = Value
  val cardEventSpecial = Value

  object EventStates {
    val cardEventStates = Set(
      cardEventInfluence, cardEventSelectCardOrCancel, cardEventYesNo,
      cardEventSelectCountry, cardEventOperation, cardEventConfirm,
      cardEventSelectCard, cardEventAnotherCard, cardEventSelectMultipleCards,
      cardEventSpecial
    )
    def unapply(state: State): Option[Boolean] = {
      if (cardEventStates(state)) Some(true) else None
    }
  }

}
