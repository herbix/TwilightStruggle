package me.herbix.ts.logic.card

import me.herbix.ts.logic.Faction.{Faction, _}
import me.herbix.ts.logic.State._
import me.herbix.ts.logic._
import me.herbix.ts.util.ConditionBuilder._
import me.herbix.ts.util.OperationHint

/**
  * Created by Chaofan on 2016/7/24.
  */

object Card104CambridgeFive extends CardMultiStep(104, 2, USSR, false) {
  override def canEvent(game: Game, faction: Faction) = game.turn <= 7

  @prepare
  def showCards(game: Game): Unit = {
    val eventCards = new CardSet(game)
    game.hand(US).filter(!_.canHeld(game)).foreach(eventCards.add)
    game.currentCardData = eventCards
    game.clearSnapshots()
  }

  @step1(cardEventSelectCardOrCancel, selectCard.all)
  def selectScoringCard(game: Game, input: Operation): Int = {
    val cardOption = input.asInstanceOf[OperationSelectCard].card
    game.currentCardData = cardOption match {
      case Some(c: CardScoring) => c.region
      case Some(Card038SEAsiaScoring) => Region.SouthEastAsia
      case _ => null
    }
    if (cardOption.isDefined) 2 else 3
  }

  @step2(cardEventInfluence, 1, true, true, USSR, influence.inRegionShownInCardData)
  def addUSSRInfluence(game: Game, input: Operation): Unit = {
    game.modifyInfluence(USSR, true, input.asInstanceOf[OperationModifyInfluence].detail)
  }
}

object Card105SpecialRelationship extends CardMultiStep(105, 2, US, false) {
  @prepare
  def checkUKAndNATO(game: Game, input: Operation): Int = {
    val uk = game.theWorldMap.countries("UK")
    if (game.getController(uk) == US) {
      if (game.flags.hasFlag(Flags.NATO)) 2 else 1
    } else {
      3
    }
  }

  @step1(cardEventSelectCountry, 1, true, selectCountry.withName("Canada", "Benelux", "France", "Norway"))
  def add1USInfluence(game: Game, input: Operation): Int = {
    val country = input.asInstanceOf[OperationSelectCountry].detail.head
    game.modifyInfluence(US, true, Map(country -> 1))
    3
  }

  @step2(cardEventSelectCountry, 1, true, selectCountry.inRegion(Region.WestEurope))
  def add2USInfluence(game: Game, input: Operation): Int = {
    val country = input.asInstanceOf[OperationSelectCountry].detail.head
    game.modifyInfluence(US, true, Map(country -> 2))
    game.addVpAndCheck(US, 2)
    3
  }
}

object Card106NORAD extends CardInstant(106, 3, US, true) {
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    game.addFlag(US, Flags.NORAD, game.defcon)
    true
  }
}

object Card107Che extends CardMultiStep(107, 3, USSR, false) {
  @prepare
  def saveOp(game: Game): Unit = {
    game.currentCardData = game.modifyOp(faction, this.op)
  }

  @step1(cardEventSelectCountry, 1, false, selectCountry
    .inRegion(Region.MidAmerica, Region.SouthAmerica, Region.Africa).notBattlefield.canCoupWithoutFlags)
  def doCoup(game: Game, input: Operation): Int = {
    val modifier = game.currentCardData.asInstanceOf[Int]
    var secondCoup = false
    val detail = input.asInstanceOf[OperationSelectCountry].detail
    for (country <- detail) {
      if (game.coup(country, USSR, modifier) > 0) {
        secondCoup = true
      }
      game.addMilitary(USSR, modifier)
    }
    if (secondCoup) {
      game.currentCardData = (game.currentCardData, detail.head)
      2
    } else {
      game.currentCardData = null
      3
    }
  }

  @step2(cardEventSpecial)
  def doCoup2(game: Game, input: Operation): Unit = {
    val modifier = game.currentCardData.asInstanceOf[(Int, Country)]._1
    for (country <- input.asInstanceOf[OperationSelectCountry].detail) {
      game.coup(country, USSR, modifier)
      game.addMilitary(USSR, modifier)
    }
    game.currentCardData = null
  }

  override def getSpecialOperationHint(game: Game): OperationHint = {
    val rest = game.currentCardData.asInstanceOf[(Int, Country)]._1
    val validCheck =
      selectCountry
        .inRegion(Region.MidAmerica, Region.SouthAmerica, Region.Africa)
        .notBattlefield
        .canCoupWithoutFlags
        .extraCondition((game, detail) => !detail.contains(game.currentCardData.asInstanceOf[(Int, Country)]._2))
        .build
    OperationHint(classOf[OperationSelectCountry], 1, rest, validCheck, false)
  }
}

object Card108OurManInTehran extends CardMultiStep(108, 2, US, true) {
  override def canEvent(game: Game, faction: Faction) =
    game.theWorldMap.countries.values.exists(c => game.getController(c) == US && c.regions(Region.MidEast))

  @prepare
  def pick5CardsFromDeck(game: Game, input: Operation): Unit = {
    val eventCards = new CardSet(game)
    for (i <- 1 to 5) {
      eventCards.add(game.pickCardFromDeck())
    }
    game.currentCardData = eventCards
  }

  @step1(cardEventSelectMultipleCards, selectCard.canDiscard)
  def discardDeckCards(game: Game, input: Operation): Unit = {
    val eventCards = game.currentCardData.asInstanceOf[CardSet]
    for (card <- input.asInstanceOf[OperationSelectCards].cards) {
      eventCards.remove(card)
      game.discardCard(card, US, true, true)
    }
    for (card <- eventCards) {
      game.deckAdd(card)
    }
    game.currentCardData = null
  }
}

object Card109YuriAndSamantha extends CardInstant(109, 2, USSR, true) {
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    game.addFlag(USSR, Flags.Samantha)
    true
  }
}

object Card110AwacsSale extends CardInstant(110, 3, US, true) {
  override def instantEvent(game: Game, faction: Faction): Boolean = {
    val saudiArabia = game.theWorldMap.countries("Saudi Arabia")
    game.modifyInfluence(US, true, Map(saudiArabia -> 2))
    game.addFlag(US, Flags.AwacsSale)
    true
  }
}
